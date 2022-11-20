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
  ;; The expr over which the proxy abstracts.
  (expr nil :read-only t)
  ;; A vector of symbols or literal objects - one for each value that the
  ;; proxy's expr produces.
  (values nil :type simple-vector :read-only t)
  ;; A values type specifier for the values returned by the proxy, or NIL
  ;; if nothing is known about the proxies values.
  (type '(values) :type list :read-only t))

(defun proxy-value (proxy &optional (value-n 0))
  (declare (proxy proxy))
  (svref (proxy-values proxy) value-n))

(defun proxy-ref (proxy &optional (value-n 0))
  (if (and (eql value-n 0)
           (eql (length (proxy-values proxy)) 1))
      proxy
      (make-proxy
       :level (proxy-level proxy)
       :values (vector (elt (proxy-values proxy) value-n))
       :expr `(nth-value ,value-n ,(proxy-expr proxy))
       :type `(values ,(nth (1+ value-n) (proxy-type proxy)) &optional))))

(defun constant-proxy (form)
  (let ((value (if (constantp form) form `',form)))
    (make-proxy
     :level 0
     :values (vector value))))

(define-compiler-macro constant-proxy (&whole whole form)
  (if (constantp form)
      `(load-time-value
        (locally (declare (notinline constant-proxy))
          (constant-proxy ,form)))
      whole))

(defun make-index-proxy-vector (n)
  (let ((vector (make-array n)))
    (dotimes (index n vector)
      (setf (svref vector index)
            (make-proxy
             :level (1+ index)
             :values (vector (gensym "INDEX")))))))

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

;; A vector of vectors of proxies - one vector of proxies for each target
;; array.
(defvar *target-index-proxies-vector*)

;; A vector of vectors of proxies - one vector of proxies for each source
;; array.
(defvar *source-index-proxies-vector*)

(defun translate-blueprint (blueprint)
  (trivia:ematch (ucons:tree-from-utree blueprint)
    ((list* iteration-space targets sources instructions)
     (let* ((rank (length iteration-space))
            (*index-proxies* (make-index-proxy-vector rank))
            (*rproxies-vector* (make-array (1+ rank) :initial-element '()))
            (*instructions* (coerce instructions 'simple-vector)))
       (multiple-value-bind (wrap-in-loop-vector)
           (translate-iteration-space iteration-space)
         (multiple-value-bind (*target-array-proxies*
                               *target-stride-proxies-vector*
                               *target-index-proxies-vector*)
             (translate-array-info targets :targets)
           (multiple-value-bind (*source-array-proxies*
                                 *source-stride-proxies-vector*
                                 *source-index-proxies-vector*)
               (translate-array-info sources :sources)
             ;; Now convert instructions to proxies.
             (loop for instruction across *instructions*
                   for instruction-number from 0
                   do (setf (svref *instructions* instruction-number)
                            (translate-instruction instruction-number)))
             ;; Finally, convert the proxies of each level to suitable
             ;; bindings and apply the loop wrappers.
             `(lambda (.kernel. .iteration-space. &optional (.buffer-storage. #'buffer-storage))
                (declare (ignorable .iteration-space.))
                (declare (kernel .kernel.))
                (declare (function .buffer-storage.))
                (with-unsafe-optimizations
                  (let ((.instructions. (kernel-instruction-vector .kernel.)))
                    (declare (ignorable .instructions.))
                    ,(let ((body '(values)))
                       (loop for axis from rank downto 0 do
                         (let ((proxies (reverse (aref *rproxies-vector* axis))))
                           (setf body `(bind ,@(mapcar #'proxy-binding proxies)
                                         (() ,body (values)))))
                         (setf body (funcall (aref wrap-in-loop-vector axis) body)))
                       body)))))))))))

(defun meta-funcall (number-of-values operator-proxy &rest argument-proxies)
  (let* ((level (reduce #'max (list* operator-proxy argument-proxies) :key #'proxy-level))
         (arguments (mapcar #'proxy-value argument-proxies))
         (expr
           (trivia:ematch (proxy-value operator-proxy)
             ((list 'quote (list 'setf function-name))
              `(setf (,function-name ,@(rest arguments)) ,(first arguments)))
             ((list 'quote (and function-name (type symbol)))
              `(,function-name ,@arguments))
             (operator
              `(funcall ,operator ,@arguments)))))
    (ensure-proxy
     :level level
     :number-of-values number-of-values
     :expr expr)))

(defun ensure-proxy
    (&key
       (level 0)
       (number-of-values 1)
       (expr (alexandria:required-argument :expr))
       (type '(values))
       (name "V"))
  (symbol-macrolet ((rproxies (svref *rproxies-vector* level)))
    ;; Now we either reuse the proxy of an existing expr, or create
    ;; and register a new one.
    (or (find-if
         (lambda (proxy)
           (assert (= level (proxy-level proxy)))
           (and (equal expr (proxy-expr proxy))
                (equal type (proxy-type proxy))
                (<= number-of-values (length (proxy-values proxy)))))
         rproxies)
        (let ((proxy (make-proxy
                      :type type
                      :level level
                      :values (make-gensym-vector number-of-values name)
                      :expr expr)))
          (push proxy rproxies)
          proxy))))

(defun make-gensym-vector (n &optional (varname "G"))
  (let ((vector (make-array n))
        (string (string varname)))
    (dotimes (index n vector)
      (setf (svref vector index)
            (gensym string)))))

(defun meta-index-+ (&rest argument-proxies)
  (let ((fn (constant-proxy 'index-+))
        (zero (constant-proxy 0)))
    (trivia:match (remove 0 argument-proxies :key #'proxy-value)
      ((list) zero)
      ((list proxy) proxy)
      (proxies
       (reduce
        (lambda (a b)
          (meta-funcall 1 fn a b))
        (stable-sort (copy-list proxies) #'< :key #'proxy-level))))))

(defun meta-index-* (&rest argument-proxies)
  (let ((fn (constant-proxy 'index-*))
        (zero (constant-proxy 0))
        (one (constant-proxy 1)))
    (if (member 0 argument-proxies :key #'proxy-value)
        zero
        (trivia:match (remove 1 argument-proxies :key #'proxy-value)
          ((list) one)
          ((list proxy) proxy)
          (proxies
           (reduce
            (lambda (a b)
              (meta-funcall 1 fn a b))
            (stable-sort (copy-list proxies) #'< :key #'proxy-level)))))))

(defun translate-instruction (instruction-number)
  (let ((instruction (svref *instructions* instruction-number)))
    (trivia:ematch instruction
      ((list* :call number-of-values operator arguments)
       (apply
        #'meta-funcall
        number-of-values
        (if (eql operator :any)
            (ensure-proxy
             :name "FN"
             :expr
             `(call-instruction-operator
               ,(proxy-value
                 (ensure-proxy
                  :expr `(aref .instructions. ,instruction-number)
                  :name "CALL"))))
            (constant-proxy operator))
        (loop for (value-n instruction-number) in arguments
              collect
              (proxy-ref (aref *instructions* instruction-number) value-n))))
      ((list* :load array-number transformation-number offsets)
       (meta-funcall
        1
        (constant-proxy 'row-major-aref)
        (svref *source-array-proxies* array-number)
        (let* ((stride-proxies (aref *source-stride-proxies-vector* array-number))
               (index-proxy (aref (aref *source-index-proxies-vector* array-number)
                                  transformation-number)))
          (apply #'meta-index-+
                   index-proxy
                   (loop for offset in offsets
                         for stride-proxy across stride-proxies
                         collect (meta-index-* stride-proxy (constant-proxy offset)))))))
      ((list :store (list value-n instruction-number) array-number transformation-number)
       (meta-funcall
        0
        (constant-proxy '(setf row-major-aref))
        (proxy-ref (aref *instructions* instruction-number) value-n)
        (svref *target-array-proxies* array-number)
        (aref (aref *target-index-proxies-vector* array-number) transformation-number)))
      ((list :iref (list permutation scaling))
       (let* ((instruction
                (proxy-value
                 (ensure-proxy
                  :expr `(aref .instructions. ,instruction-number)
                  :name "IREF")))
              (transformation
                (proxy-value
                 (ensure-proxy
                  :expr `(iref-instruction-transformation ,instruction)
                  :name "TRANSFORMATION")))
              (offset-proxy
                (ensure-proxy
                 :expr `(aref (transformation-offsets ,transformation) 0)
                 :name "OFFSET")))
         (if (not permutation)
             offset-proxy
             (meta-index-+
              (meta-index-*
               (aref *index-proxies* permutation)
               (if (eql scaling :any)
                   (ensure-proxy
                    :expr `(aref (transformation-scalings ,transformation) 0)
                    :name "SCALING")
                   (constant-proxy scaling)))
              offset-proxy)))))))

(defun proxy-binding (proxy)
  `(,(loop for value across (proxy-values proxy)
           collect
           (if (symbolp value) value (gensym)))
    ,(proxy-expr proxy)
    ,(proxy-type proxy)))

(defmacro bind (&body bindings)
  (labels ((expand (bindings)
             (trivia:match bindings
               ((list) `(values))
               ((list* (list variables form type) rest)
                (trivia:match variables
                  ((list)
                   `(progn (the ,type ,form)
                           ,(expand rest)))
                  ((list variable)
                   `(let ((,variable (the ,type ,form)))
                      (declare (ignorable ,variable))
                      ,(expand rest)))
                  ((list* variables)
                   `(multiple-value-bind ,variables
                        (the ,type ,form)
                      (declare (ignorable ,@variables))
                      ,(expand rest))))))))
    (expand bindings)))

(defun translate-iteration-space (iteration-space)
  (let* ((rank (length iteration-space))
         (ranges-proxy
           (ensure-proxy
            :number-of-values rank
            :expr `(values-list (shape-ranges .iteration-space.))
            :name "RANGE"))
         (wrap-in-loop-vector (make-array (1+ rank))))
    ;; The outermost 'loop' is the top level
    (setf (svref wrap-in-loop-vector 0) #'identity)
    (loop for info in iteration-space
          for axis from 0
          do (let* ((index (proxy-value (aref *index-proxies* axis)))
                    (range (proxy-value ranges-proxy axis))
                    (start (proxy-value (ensure-proxy :expr `(range-start ,range) :name "START")))
                    (step (proxy-value (ensure-proxy :expr `(range-step ,range) :name "STEP")))
                    (end (proxy-value (ensure-proxy :expr `(range-end ,range) :name "END"))))
               (setf (svref wrap-in-loop-vector (1+ axis))
                     (if (eql info :contiguous)
                         (lambda (form)
                           `(loop :for ,index fixnum :from ,start :below ,end
                                  :do ,form))
                         (lambda (form)
                           `(loop :for ,index fixnum :from ,start :by ,step :below ,end
                                  :do ,form))))))
    (values wrap-in-loop-vector)))

(defun translate-array-info (array-info kind)
  (let* ((n (length array-info))
         (array-proxies (make-array n))
         (stride-proxies-vector (make-array n))
         (index-proxies-vector (make-array n))
         (entries-proxy
           (ensure-proxy
            :number-of-values n
            :name "ENTRY"
            :expr
            `(values-list
              ,(ecase kind
                 (:sources `(kernel-sources .kernel.))
                 (:targets `(kernel-targets .kernel.)))))))
    ;; Loop over each array.
    (loop
      for (ntype . list-of-irefs) in array-info
      for array-index from 0 below n
      for entry-proxy = (proxy-ref entries-proxy array-index)
      do (let* ((buffer-proxy (ensure-proxy
                               :expr `(first ,(proxy-value entry-proxy))
                               :name "BUFFER"))
                (element-type (petalisp.type-inference:type-specifier ntype))
                (array-proxy (ensure-proxy
                              :name (ecase kind (:sources "SRC") (:targets "DST"))
                              :expr `(funcall .buffer-storage. ,(proxy-value buffer-proxy))
                              :type `(values (simple-array ,element-type) &optional)))
                (n-refs (length list-of-irefs))
                (ref-type (ecase kind (:sources 'stencil) (:targets 'store-instruction)))
                (refs-proxy (ensure-proxy
                             :name (ecase kind (:sources "STENCIL") (:targets "STORE"))
                             :number-of-values n-refs
                             :expr `(values-list (rest ,(proxy-value entry-proxy)))
                             :type `(values ,@(loop repeat n-refs collect ref-type) &optional)))
                (rank (length (first list-of-irefs)))
                (stride-proxies (make-array rank))
                (index-proxies (make-array n-refs)))
           (setf (svref array-proxies array-index) array-proxy)
           (setf (svref stride-proxies-vector array-index) stride-proxies)
           (setf (svref index-proxies-vector array-index) index-proxies)
           ;; Bind the array's strides.
           (unless (zerop rank)
             (setf (svref stride-proxies (1- rank))
                   (constant-proxy 1))
             ;; The strides of all previous axes are the product of the
             ;; stride of the previous axis and the size of that axis.
             (loop for axis from (- rank 2) downto 0
                   for expr = `(array-dimension ,(proxy-value array-proxy) ,(1+ axis))
                   do (setf (svref stride-proxies axis)
                            (meta-index-*
                             (ensure-proxy :expr expr :name "DIM")
                             (svref stride-proxies (1+ axis))))))
           ;; Loop over each reference into that array.
           (loop
             for ref-index from 0 below n-refs
             for irefs in list-of-irefs
             do (let* ((ref-proxy (proxy-ref refs-proxy ref-index))
                       (ref-value (proxy-value ref-proxy)))
                  (multiple-value-bind (offsets-proxy scalings-proxy)
                      (ecase kind
                        (:sources
                         (values (ensure-proxy
                                  :expr `(stencil-center ,ref-value)
                                  :name "CENTER")
                                 (ensure-proxy
                                  :expr `(stencil-scalings ,ref-value)
                                  :name "SCALINGS")))
                        (:targets
                         (let* ((transformation-proxy
                                  (ensure-proxy
                                   :expr `(store-instruction-transformation ,ref-value)
                                   :name "TRANSFORMATION"))
                                (tr (proxy-value transformation-proxy)))
                           (values (ensure-proxy
                                    :expr `(transformation-offsets ,tr)
                                    :name "OFFSETS")
                                   (ensure-proxy
                                    :expr `(transformation-scalings ,tr)
                                    :name "SCALINGS")))))
                    (setf (aref index-proxies ref-index)
                          (apply
                           #'meta-index-+
                             ;; Loop over each output index of that reference.
                           (loop
                             for (permutation scaling) in irefs
                             for axis from 0
                             for stride-proxy across stride-proxies
                             collect
                             (meta-index-*
                              stride-proxy
                              (ensure-proxy
                               :expr `(aref ,(proxy-value offsets-proxy) ,axis)
                               :name "OFFSET"))
                             collect
                             (if (null permutation)
                                 (constant-proxy 0)
                                 (meta-index-*
                                  stride-proxy
                                  (if (eql scaling :any)
                                      (ensure-proxy
                                       :expr `(aref ,(proxy-value scalings-proxy) ,axis)
                                       :name "SCALING")
                                      (constant-proxy scaling))
                                  (svref *index-proxies* permutation)))))))))))
    (values
     array-proxies
     stride-proxies-vector
     index-proxies-vector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Macros

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
