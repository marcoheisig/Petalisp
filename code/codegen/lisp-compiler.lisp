;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

;;; The code in this file handles the translation of a blueprint to a compiled
;;; kernel.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Proxies
;;;
;;; A proxy describes the values that would be produced if the kernel would
;;; execute a certain expression.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Memory References
;;;
;;; A memory reference tracks all proxies and metadata related to a particular
;;; source or target.

(defstruct memref
  ;; The ntype of each element being referenced.
  (ntype nil :type typo:ntype)
  ;; The rank of the storage being referenced.
  (rank nil :type rank)
  ;; The proxy for the base of this memory region.
  (base-proxy nil :type proxy)
  ;; The proxy for the offset of this memory region.
  (offset-proxy nil :type proxy)
  ;; A vector of one proxy for each axis of the memory region.
  (stride-proxies nil :type (simple-array proxy (*)))
  ;; A vector of one proxy for each transformation into the reference.
  (index-proxies nil :type (simple-array proxy (*))))

(defun memref-stride-proxy (memref axis)
  (declare (memref memref))
  (aref (memref-stride-proxies memref) axis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translation

;; A vector filled with instruction blueprints, which are later replaced by
;; proxies.
(defvar *instructions*)

;; A vector of reversed lists of proxies - one more than the kernel has loops.
(defvar *rproxies-vector*)

;; A vector of proxies - one for the index of each loop.
(defvar *index-proxies*)

;; A vector with the memref corresponding to each target.
(defvar *target-memrefs*)

;; A vector with the memref corresponding to each source.
(defvar *source-memrefs*)

;; Count the number of call instructions whose operator is :ANY (meaning the
;; function is not hard-coded, but supplied from the outside).
(defvar *function-counter*)

(defun next-function-index ()
  (prog1 *function-counter*
    (incf *function-counter*)))

;; An object that is supplied as a first argument to several generic
;; translation functions, so that users can define more specialized methods.
(defvar *client*)

(defun translate-blueprint (client blueprint)
  (trivia:ematch (ucons:tree-from-utree blueprint)
    ((list* iteration-space targets sources instructions)
     (let* ((rank (length iteration-space))
            (*client* client)
            (*index-proxies* (make-index-proxy-vector rank))
            (*rproxies-vector* (make-array (1+ rank) :initial-element '()))
            (*instructions* (coerce instructions 'simple-vector))
            (*target-memrefs* (translate-memrefs targets :targets))
            (*source-memrefs* (translate-memrefs sources :sources))
            (*function-counter* 0)
            (wrap-in-loop-vector (translate-iteration-space iteration-space)))
       ;; Translate all instructions.
       (loop for instruction across *instructions*
             for instruction-number from 0
             do (setf (svref *instructions* instruction-number)
                      (translate-instruction instruction-number)))
       ;; Convert the proxies of each level to suitable bindings and apply the
       ;; loop wrappers.
       `(lambda (.kernel. .iteration-space. .targets. .sources. .environment.
                 &aux (.instructions. (kernel-instruction-vector .kernel.)))
          (declare (kernel .kernel.))
          (declare (shape .iteration-space.))
          (declare (ignorable .kernel. .iteration-space. .targets. .sources. .instructions. .environment.))
          (with-unsafe-optimization
            ,(let ((body '(values)))
               (loop for axis from rank downto 0 do
                 (let ((proxies (reverse (aref *rproxies-vector* axis))))
                   (setf body `(bind ,@(mapcar #'proxy-binding proxies)
                                 (() ,body (values)))))
                 (setf body (funcall (aref wrap-in-loop-vector axis) body)))
               body)))))))

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

(defun meta-index+ (&rest argument-proxies)
  (let ((fn (constant-proxy 'index+))
        (zero (constant-proxy 0)))
    (trivia:match (remove 0 argument-proxies :key #'proxy-value)
      ((list) zero)
      ((list proxy) proxy)
      (proxies
       (reduce
        (lambda (a b)
          (meta-funcall 1 fn a b))
        (stable-sort (copy-list proxies) #'< :key #'proxy-level))))))

(defun meta-index* (&rest argument-proxies)
  (let ((fn (constant-proxy 'index*))
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
             `(call-instruction-function
               ,(proxy-value
                 (ensure-proxy
                  :expr `(aref .instructions. ,instruction-number)
                  :name "CALL"))))
            (constant-proxy operator))
        (loop for (value-n instruction-number) in arguments
              collect
              (proxy-ref (aref *instructions* instruction-number) value-n))))
      ((list* :load array-number transformation-number offsets)
       (with-slots (ntype rank base-proxy stride-proxies index-proxies)
           (aref *source-memrefs* array-number)
         (meta-funcall
          1
          (constant-proxy (load-function *client* ntype))
          base-proxy
          (apply
           #'meta-index+
           (aref index-proxies transformation-number)
           (loop for offset in offsets
                 for stride-proxy across stride-proxies
                 collect (meta-index* stride-proxy (constant-proxy offset)))))))
      ((list :store (list value-n instruction-number) array-number transformation-number)
       (with-slots (ntype rank base-proxy stride-proxies index-proxies)
           (aref *target-memrefs* array-number)
         (meta-funcall
          0
          (constant-proxy (store-function *client* ntype))
          (proxy-ref (aref *instructions* instruction-number) value-n)
          base-proxy
          (aref index-proxies transformation-number))))
      ((list :iref (list permutation scaling))
       (let* ((instruction
                (proxy-value
                 (ensure-proxy
                  :expr `(aref .instructions. ,instruction-number)
                  :name "IREF"
                  :type '(values instruction &optional))))
              (transformation
                (proxy-value
                 (ensure-proxy
                  :expr `(iref-instruction-transformation ,instruction)
                  :name "TRANSFORMATION"
                  :type '(values transformation &optional))))
              (offset-proxy
                (ensure-proxy
                 :expr `(aref (transformation-offsets ,transformation) 0)
                 :name "OFFSET"
                 :type '(values fixnum &optional))))
         (if (not permutation)
             offset-proxy
             (meta-index+
              (meta-index*
               (aref *index-proxies* permutation)
               (if (eql scaling :any)
                   (ensure-proxy
                    :expr `(aref (transformation-scalings ,transformation) 0)
                    :name "SCALING"
                    :type '(values fixnum &optional))
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
                    (start (proxy-value (ensure-proxy :expr `(range-start ,range) :name "START" :type '(values fixnum &optional))))
                    (step (proxy-value (ensure-proxy :expr `(range-step ,range) :name "STEP" :type '(values fixnum &optional))))
                    (end (proxy-value (ensure-proxy :expr `(range-end ,range) :name "END" :type '(values fixnum &optional)))))
               (setf (svref wrap-in-loop-vector (1+ axis))
                     (if (eql info :contiguous)
                         (lambda (form)
                           `(loop :for ,index fixnum :from ,start :below ,end
                                  :do ,form))
                         (lambda (form)
                           `(loop :for ,index fixnum :from ,start :by ,step :below ,end
                                  :do ,form))))))
    (values wrap-in-loop-vector)))

(defun translate-memrefs (references kind)
  (let* ((nmemrefs (length references))
         (memrefs (make-array nmemrefs)))
    (loop for reference in references for index below nmemrefs do
      (setf (aref memrefs index)
            (translate-memref reference index kind)))
    memrefs))

(defun translate-memref (reference position kind)
  (destructuring-bind (ntype . transformation-blueprints) reference
    (let* ((nstencils (length transformation-blueprints))
           (stencils-proxy
             (ensure-proxy
              :number-of-values nstencils
              :name "STENCIL"
              :expr
              `(values-list
                (cdr
                 (nth ,position
                      ,(ecase kind
                         (:sources `(kernel-sources .kernel.))
                         (:targets `(kernel-targets .kernel.))))))))
           (rank (length (first transformation-blueprints)))
           (unpack-proxy
             (multiple-value-bind (fn type)
                 (unpack-function *client* ntype rank)
               (ensure-proxy
                :number-of-values (+ rank 2)
                :expr `(,fn
                        (,(target-function *client*)
                         ,(ecase kind
                            (:sources '.sources.)
                            (:targets '.targets.))
                         ,position)
                        .environment.)
                :type type)))
           (stride-proxies (make-array rank)))
      (loop for axis below rank do
        (setf (svref stride-proxies axis)
              (if (= axis (1- rank))
                  (constant-proxy 1)
                  (proxy-ref unpack-proxy (+ axis 2)))))
      (make-memref
       :ntype ntype
       :rank rank
       :base-proxy (proxy-ref unpack-proxy 0)
       :offset-proxy (proxy-ref unpack-proxy 1)
       :stride-proxies stride-proxies
       :index-proxies
       (let ((index-proxies (make-array (length transformation-blueprints))))
         (loop
           for irefs in transformation-blueprints
           for index from 0
           for stencil-proxy = (proxy-ref stencils-proxy index)
           for stencil = (proxy-value stencil-proxy) do
             (multiple-value-bind (offsets-proxy scalings-proxy)
                 (ecase kind
                   (:sources
                    (values (ensure-proxy
                             :expr `(stencil-center ,stencil)
                             :name "CENTER")
                            (ensure-proxy
                             :expr `(stencil-scalings ,stencil)
                             :name "SCALINGS")))
                   (:targets
                    (let* ((transformation-proxy
                             (ensure-proxy
                              :expr `(store-instruction-transformation ,stencil)
                              :name "TRANSFORMATION"))
                           (tr (proxy-value transformation-proxy)))
                      (values (ensure-proxy
                               :expr `(transformation-offsets ,tr)
                               :name "OFFSETS")
                              (ensure-proxy
                               :expr `(transformation-scalings ,tr)
                               :name "SCALINGS")))))
               (setf (aref index-proxies index)
                     (apply
                      #'meta-index+
                      (proxy-ref unpack-proxy 1)
                      ;; Loop over each output index of that reference.
                      (loop
                        for (permutation scaling) in irefs
                        for axis from 0
                        for stride-proxy across stride-proxies
                        collect
                        (meta-index*
                         stride-proxy
                         (ensure-proxy
                          :expr `(aref ,(proxy-value offsets-proxy) ,axis)
                          :name "OFFSET"))
                        collect
                        (if (null permutation)
                            (constant-proxy 0)
                            (meta-index*
                             stride-proxy
                             (if (eql scaling :any)
                                 (ensure-proxy
                                  :expr `(aref ,(proxy-value scalings-proxy) ,axis)
                                  :name "SCALING")
                                 (constant-proxy scaling))
                             (svref *index-proxies* permutation))))))))
         index-proxies)))))
