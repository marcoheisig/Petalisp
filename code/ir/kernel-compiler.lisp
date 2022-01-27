;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The code in this file handles the translation of a blueprint to the
;;; s-expression of a function with two arguments - a kernel with the
;;; supplied blueprint and the iteration space to be executed.
;;;
;;; The translation uses two concepts - proxies and wrappers.  A proxy
;;; describes the values that would be produced if the kernel would execute
;;; a certain expression.  A wrapper is a function that takes a form and
;;; wraps it in suitable bindings such that the variables described by some
;;; proxies are defined.

(defstruct proxy
  ;; The index of the innermost loop that the proxy depends on.  The level
  ;; of a proxy that doesn't depend on any loop has a level of zero.
  (level nil :type unsigned-byte :read-only t)
  ;; A vector of symbols - one for each value that the proxy's expression
  ;; produces.
  (variables nil :type simple-vector :read-only t)
  ;; The expression over which the proxy abstracts.
  (expression nil :read-only t))

(defun proxy-variable (proxy &optional (value-n 0))
  (declare (proxy proxy))
  (svref (proxy-variables proxy) value-n))

(defun proxy-ref (proxy &optional (value-n 0))
  (declare (proxy proxy))
  (make-proxy
   :level (proxy-level proxy)
   :variables (vector (proxy-variable proxy value-n))))

(defun make-proxy-vector (n &optional (varname "G"))
  (let ((vector (make-array n))
        (string (string varname)))
    (dotimes (index n vector)
      (setf (svref vector index)
            (make-proxy
             :level 0
             :variables (vector (gensym string)))))))

(defun make-index-proxy-vector (n)
  (let ((vector (make-array n)))
    (dotimes (index n vector)
      (setf (svref vector index)
            (make-proxy
             :level (1+ index)
             :variables (vector (gensym "INDEX")))))))

(defun make-gensym-vector (n &optional (varname "G"))
  (let ((vector (make-array n))
        (string (string varname)))
    (dotimes (index n vector)
      (setf (svref vector index)
            (gensym string)))))

;; A vector filled with instruction blueprints, which are later replaced by
;; proxies.
(defvar *instructions*)

;; A vector of reversed lists of proxies - one more than the kernel has loops.
(defvar *rproxies-vector*)

;; A vector of proxies - one for the index of each loop.
(defvar *index-proxies*)

;; A vector of proxies - one for each array being written to.
(defvar *target-array-proxies*)

;; A vector of proxies - one for each array being read from.
(defvar *source-array-proxies*)

;; A vector of vectors of proxies - one vector of proxies for each target
;; array.
(defvar *target-stride-proxies-vector*)

;; A vector of vectors of proxies - one vector of proxies for each source
;; array.
(defvar *source-stride-proxies-vector*)

;; A vector of vectors of proxies - one vector of proxies for each
;; instruction.
(defvar *offset-proxies-vector*)

;; A vector of vectors of proxies - one vector of proxies for each
;; instruction.
(defvar *scaling-proxies-vector*)

;; A vector of proxies - one for each instruction.
(defvar *function-proxies*)

(defun translate-blueprint (blueprint)
  (trivia:ematch blueprint
    ((ucons:ulist* iteration-space target-array-info source-array-info
                   instruction-info)
     (let* ((rank (ucons:ulength iteration-space))
            (*rproxies-vector* (make-array (1+ rank) :initial-element '()))
            (*instructions* (ucons:vector-from-ulist (ucons:unthcdr 3 blueprint))))
       (multiple-value-bind (*index-proxies*
                             wrap-in-iteration-space-bindings
                             wrap-in-loop-vector)
           (translate-iteration-space iteration-space)
         (multiple-value-bind (*target-array-proxies*
                               *target-stride-proxies-vector*
                               wrap-in-target-array-bindings)
             (translate-array-info target-array-info '(kernel-targets .kernel.))
           (multiple-value-bind (*source-array-proxies*
                                 *source-stride-proxies-vector*
                                 wrap-in-source-array-bindings)
               (translate-array-info source-array-info '(kernel-sources .kernel.))
             (multiple-value-bind (*offset-proxies-vector*
                                   *scaling-proxies-vector*
                                   wrap-in-transformation-bindings)
                 (translate-transformation-info instruction-info)
               (multiple-value-bind (*function-proxies*
                                     wrap-in-function-bindings)
                   (translate-function-info instruction-info)
                 ;; Now convert instructions to proxies.
                 (loop for instruction across *instructions*
                       for instruction-number from 0
                       do (setf (svref *instructions* instruction-number)
                                (translate-instruction instruction-number)))
                 ;; Finally, convert the proxies of each level to suitable
                 ;; bindings and apply all wrappers.
                 `(lambda (.kernel. .iteration-space. &optional (.buffer-storage. #'buffer-storage))
                    (declare (ignorable .iteration-space.))
                    (declare (kernel .kernel.))
                    (declare (function .buffer-storage.))
                    (with-unsafe-optimizations
                      (let ((.instructions. (kernel-instruction-vector .kernel.)))
                        (declare (ignorable .instructions.))
                        ,(funcall
                          (apply
                           #'alexandria:compose
                           wrap-in-iteration-space-bindings
                           wrap-in-target-array-bindings
                           wrap-in-source-array-bindings
                           wrap-in-transformation-bindings
                           wrap-in-function-bindings
                           ;; Alternate loops and proxy forms.
                           (loop for rproxies across *rproxies-vector*
                                 for wrap-in-loop across wrap-in-loop-vector
                                 collect wrap-in-loop
                                 collect (translate-proxies (reverse rproxies))))
                          `(values))))))))))))))

(defun ensure-proxy (number-of-values operator &rest inputs)
  (let ((rargs '())
        (level 0))
    ;; We allow for three different kinds of inputs - ulists, proxies and
    ;; constant integers.
    (dolist (input inputs)
      (trivia:ematch input
        ((ucons:ulist value-n instruction-number)
         (let ((input-proxy (svref *instructions* instruction-number)))
           (push (proxy-variable input-proxy value-n)
                 rargs)
           (setf level (max level (proxy-level input-proxy)))))
        ((type proxy)
         (push (proxy-variable input) rargs)
         (setf level (max level (proxy-level input))))
        ((type integer)
         (push input rargs))
        ;; Sometimes, a rational number creeps into kernel blueprints, but
        ;; only in case it is never used.  So we can safely replace it with
        ;; zero.
        ((type ratio)
         (push 0 rargs))))
    (symbol-macrolet ((rproxies (svref *rproxies-vector* level)))
      (let ((expression
              (etypecase operator
                (symbol `(,operator ,@(reverse rargs)))
                (proxy `(funcall ,(proxy-variable operator) ,@(reverse rargs))))))
        ;; Now we either reuse the proxy of an existing expression, or
        ;; create and register a new one.
        (or (find expression rproxies
                  :key #'proxy-expression
                  :test #'equal)
            (let ((proxy (make-proxy
                          :level level
                          :variables (make-gensym-vector number-of-values "VALUE")
                          :expression expression)))
              (push proxy rproxies)
              proxy))))))

(defun translate-instruction (instruction-number)
  (let ((instruction (svref *instructions* instruction-number)))
    (trivia:ematch instruction
      ((ucons:ulist* :call number-of-values operator arguments)
       (apply
        #'ensure-proxy
        number-of-values
        (or operator (svref *function-proxies* instruction-number))
        (ucons:list-from-ulist arguments)))
      ((ucons:ulist* :load array-number irefs)
       (ensure-proxy
        1
        'row-major-aref
        (svref *source-array-proxies* array-number)
        (translate-row-major-index
         (svref *source-stride-proxies-vector* array-number)
         (svref *offset-proxies-vector* instruction-number)
         (svref *scaling-proxies-vector* instruction-number)
         irefs)))
      ((ucons:ulist* :store input array-number irefs)
       (ensure-proxy
        0
        'setf-row-major-aref
        input
        (svref *target-array-proxies* array-number)
        (translate-row-major-index
         (svref *target-stride-proxies-vector* array-number)
         (svref *offset-proxies-vector* instruction-number)
         (svref *scaling-proxies-vector* instruction-number)
         irefs)))
      ((ucons:ulist* :iref irefs)
       (translate-row-major-index
        (vector 1)
        (svref *offset-proxies-vector* instruction-number)
        (svref *scaling-proxies-vector* instruction-number)
        irefs)))))

(defun translate-row-major-index
    (stride-proxies offset-proxies scaling-proxies irefs)
  (let ((summands '()))
    (loop for ulist = irefs then (ucons:ucdr ulist)
          until (null ulist)
          for offset-proxy across offset-proxies
          for scaling-proxy across scaling-proxies
          for stride-proxy across stride-proxies
          do (trivia:ematch (ucons:ucar ulist)
               ((ucons:ulist permutation scaling offset)
                (push (ensure-proxy 1 'index-* (or offset offset-proxy) stride-proxy)
                      summands)
                (unless (null permutation)
                  (push
                   (ensure-proxy
                    1 'index-*
                    (ensure-proxy 1 'index-* stride-proxy (or scaling scaling-proxy))
                    (svref *index-proxies* permutation))
                   summands)))))
    (reduce
     (lambda (&rest proxies)
       (apply #'ensure-proxy 1 'index-+ proxies))
     (stable-sort summands #'< :key #'proxy-level))))

(defun translate-proxies (proxies)
  (lambda (form)
    (labels ((wrap-in-proxies (proxies)
               (if (null proxies)
                   form
                   (let ((proxy (first proxies))
                         (rest (rest proxies)))
                     (trivia:match (coerce (proxy-variables proxy) 'list)
                       ((list)
                        `(progn ,(proxy-expression proxy)
                                ,(wrap-in-proxies rest)))
                       ((list variable)
                        `(let ((,variable ,(proxy-expression proxy)))
                           ,(wrap-in-proxies rest)))
                       ((list* variables)
                        `(multiple-value-bind ,variables
                             ,(proxy-expression proxy)
                           (declare (ignorable ,@variables))
                           ,(wrap-in-proxies rest))))))))
      (wrap-in-proxies proxies))))

(defun translate-iteration-space (iteration-space-info)
  (let* ((rank (ucons:ulength iteration-space-info))
         (ranges (loop repeat rank collect (gensym "RANGE")))
         (starts (loop repeat rank collect (gensym "START")))
         (steps (loop repeat rank collect (gensym "STEP")))
         (ends (loop repeat rank collect (gensym "END")))
         (index-proxies (make-index-proxy-vector rank))
         (wrap-in-loop-vector (make-array (1+  rank))))
    ;; The outermost 'loop' is the top level
    (setf (svref wrap-in-loop-vector 0) #'identity)
    (loop for index-proxy across index-proxies
          for range in ranges
          for start in starts
          for step in steps
          for end in ends
          for axis from 0
          for ulist = iteration-space-info then (ucons:ucdr ulist)
          for info = (ucons:ucar ulist)
          do (let ((index (proxy-variable index-proxy))
                   ;; Rebind the loop variables so that they can be closed
                   ;; over correctly.
                   (start start)
                   (step step)
                   (end end))
               (setf (svref wrap-in-loop-vector (1+ axis))
                     (if (eql info :contiguous)
                         (lambda (form)
                           `(loop for ,index fixnum from ,start below ,end
                                  do ,form))
                         (lambda (form)
                           `(loop for ,index fixnum from ,start by ,step below ,end do
                                  ,form))))))
    (values
     index-proxies
     (lambda (form)
       `(destructuring-bind ,ranges
            (shape-ranges .iteration-space.)
          (declare (ignorable ,@ranges))
          (let ,(loop
                  for range in ranges
                  for start in starts
                  for step in steps
                  for end in ends
                  collect `(,start (the fixnum (range-start ,range)))
                  collect `(,step (the (and fixnum unsigned-byte) (range-step ,range)))
                  collect `(,end (the fixnum (range-end ,range))))
            (declare (ignorable ,@starts ,@steps ,@ends))
            ,form)))
     wrap-in-loop-vector)))

(defun translate-array-info (array-info alist-form)
  (let* ((n (ucons:ulength array-info))
         (entries (loop repeat n collect (gensym "ENTRY")))
         (array-proxies (make-proxy-vector n "ARRAY"))
         (array-types (make-array n))
         (stride-proxies-vector (make-array n)))
    ;; Initialize the vector of array types and the vector of
    ;; stride-proxies.
    (loop for rest = array-info then (ucons:ucdr rest)
          until (null rest)
          for index from 0 do
            (trivia:ematch (ucons:ucar rest)
              ((ucons:ulist ntype rank)
               (setf (svref array-types index)
                     `(simple-array
                       ,(petalisp.type-inference:type-specifier ntype)
                       ,(loop repeat rank collect '*)))
               (setf (svref stride-proxies-vector index)
                     (make-proxy-vector rank "STRIDE")))))
    (values
     array-proxies
     stride-proxies-vector
     (lambda (form)
       `(destructuring-bind ,entries ,alist-form
          (let ,(loop for array-proxy across array-proxies
                      for entry in entries
                      collect
                      `(,(proxy-variable array-proxy)
                        (funcall .buffer-storage. (car ,entry))))
            (declare
             ,@(loop for array-proxy across array-proxies
                     for array-type across array-types
                     collect
                     `(type ,array-type ,(proxy-variable array-proxy))))
            (let* ,(loop for array-proxy across array-proxies
                         for stride-proxies across stride-proxies-vector
                         for rank = (length stride-proxies)
                         unless (zerop rank)
                           ;; The stride of the last axis is always one.
                           collect `(,(proxy-variable (svref stride-proxies (1- rank))) 1)
                         ;; The strides of all previous axes are the
                         ;; product of the stride of the previous axis and
                         ;; the size of that axis.
                         append
                         (loop for axis from (- rank 2) downto 0
                               for curr = (proxy-variable (svref stride-proxies axis))
                               for prev = (proxy-variable (svref stride-proxies (1+ axis)))
                               collect
                               `(,curr (* (array-dimension
                                           ,(proxy-variable array-proxy)
                                           ,(1+ axis))
                                          ,prev))))
              ,form)))))))

(defun translate-function-info (instruction-info)
  (let* ((n (ucons:ulength instruction-info))
         (function-proxies (make-proxy-vector n "FUNCTION"))
         (bindings
           (loop for ulist = instruction-info then (ucons:ucdr ulist)
                 until (null ulist)
                 for index from 0 below n
                 for info = (ucons:ucar ulist)
                 for function-proxy across function-proxies
                 when (and (eq (ucons:ucar info) :call)
                           (eq (ucons:unth 2 info) nil))
                   collect `(,(proxy-variable function-proxy)
                             (call-instruction-operator
                              (svref .instructions. ,index))))))
    (values
     function-proxies
     (lambda (form)
       `(let ,bindings
          (declare (function ,@(mapcar #'first bindings)))
          ,form)))))

(defun translate-transformation-info (instruction-info-ulist)
  (let* ((n (ucons:ulength instruction-info-ulist))
         (offset-proxies-vector (make-array n))
         (scaling-proxies-vector (make-array n))
         (rbindings '())
         (rdeclarations '()))
    (loop for ulist = instruction-info-ulist then (ucons:ucdr ulist)
          until (null ulist)
          for instruction-info = (ucons:ucar ulist)
          for instruction-number from 0
          do (block continue
               (multiple-value-bind (bindings declarations offset-proxies scaling-proxies)
                   (translate-irefs
                    (trivia:ematch instruction-info
                      ((ucons:ulist* :call _) (return-from continue))
                      ((ucons:ulist* :load _ irefs) irefs)
                      ((ucons:ulist* :store _ _ irefs) irefs)
                      ((ucons:ulist* :iref irefs) irefs))
                    instruction-number)
                 (setf (svref offset-proxies-vector instruction-number)
                       offset-proxies)
                 (setf (svref scaling-proxies-vector instruction-number)
                       scaling-proxies)
                 (push bindings rbindings)
                 (push declarations rdeclarations))))
    (values
     offset-proxies-vector
     scaling-proxies-vector
     (lambda (form)
       `(let* ,(apply #'append (reverse rbindings))
          (declare ,@(apply #'append (reverse rdeclarations)))
          ,form)))))

(defun translate-irefs (irefs instruction-number)
  (let* ((k (ucons:ulength irefs))
         (transformation-gensym (gensym "TRANSFORMATION"))
         (offsets-gensym (gensym "OFFSETS"))
         (scalings-gensym (gensym "SCALINGS"))
         (offset-proxies (make-proxy-vector k "OFFSET"))
         (scaling-proxies (make-proxy-vector k "SCALING"))
         (offsets
           (loop for ulist = irefs then (ucons:ucdr ulist)
                 until (null ulist)
                 collect (ucons:unth 2 (ucons:ucar ulist))))
         (scalings
           (loop for ulist = irefs then (ucons:ucdr ulist)
                 until (null ulist)
                 for index from 0
                 collect (ucons:unth 1 (ucons:ucar ulist))))
         (offsets-p (some #'not offsets))
         (scalings-p (some #'not scalings)))
    (petalisp.utilities:with-collectors ((bindings collect-binding)
                                         (declarations collect-declaration))
      (when (or offsets-p scalings-p)
        (collect-binding
         `(,transformation-gensym
           (instruction-transformation
            (svref .instructions. ,instruction-number))))
        (collect-declaration
         `(transformation ,transformation-gensym))
        (when offsets-p
          (collect-binding
           `(,offsets-gensym (transformation-offsets ,transformation-gensym)))
          (collect-declaration
           `(simple-vector ,offsets-gensym))
          (loop for offset in offsets
                for offset-proxy across offset-proxies
                for offset-variable = (proxy-variable offset-proxy)
                for index from 0
                when (not offset) do
                  (collect-binding
                   `(,offset-variable (svref ,offsets-gensym ,index)))
                  (collect-declaration
                   `(fixnum ,offset-variable))))
        (when scalings-p
          (collect-binding
           `(,scalings-gensym (transformation-scalings ,transformation-gensym)))
          (collect-declaration
           `(simple-vector ,scalings-gensym))
          (loop for scaling in scalings
                for scaling-proxy across scaling-proxies
                for scaling-variable = (proxy-variable scaling-proxy)
                for index from 0
                when (not scaling) do
                  (collect-binding
                   `(,scaling-variable (svref ,scalings-gensym ,index)))
                  (collect-declaration
                   `(fixnum ,scaling-variable)))))
      (values
       (bindings '())
       (declarations '())
       offset-proxies
       scaling-proxies))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Macros

(defmacro setf-row-major-aref (value array index)
  `(setf (row-major-aref ,array ,index)
         ,value))

(defun index-+ (&rest fixnums)
  (apply #'+ fixnums))

(defun index-* (&rest fixnums)
  (apply #'* fixnums))

(define-compiler-macro index-+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms collect `(the fixnum ,form)))))

(define-compiler-macro index-* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms collect `(the fixnum ,form)))))

(defmacro without-compiler-notes (&body body)
  "Suppress all compiler notes arising during the compilation of BODY."
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@body))

(defmacro with-unsafe-optimizations* (&body body)
  "Optimize the heck out of BODY. Use with caution!"
  (let ((settings '((speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0))))
    `(locally (declare (optimize ,@settings))
       ,@body)))

(defmacro with-unsafe-optimizations (&body body)
  "Optimize the heck out of BODY. Use with caution!

To preserve sanity, compiler efficiency hints are disabled by default. Use
WITH-UNSAFE-OPTIMIZATIONS* to see these hints."
  `(without-compiler-notes
    (with-unsafe-optimizations* ,@body)))
