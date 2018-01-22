;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/virtual-machines/common-lisp-virtual-machine
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/all
   :petalisp/core/virtual-machines/virtual-machine
   :petalisp/core/virtual-machines/compile-cache-mixin
   :petalisp/core/virtual-machines/default-scheduler-mixin)
  (:export
   #:common-lisp-virtual-machine))

(in-package :petalisp/core/virtual-machines/common-lisp-virtual-machine)

(define-class common-lisp-virtual-machine
    (virtual-machine default-scheduler-mixin compile-cache-mixin)
  ((memory-pool :type hash-table :initform (make-hash-table :test #'equalp))))

(defmethod vm/bind-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'range-size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (setf (storage immediate)
          (or
           (pop (gethash (cons element-type array-dimensions)
                         (memory-pool virtual-machine)))
           (make-array array-dimensions :element-type element-type)))))

(defmethod vm/free-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'range-size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (push (storage immediate)
          (gethash (cons element-type array-dimensions)
                   (memory-pool virtual-machine)))))

(defmethod vm/compile
  ((virtual-machine common-lisp-virtual-machine)
   (blueprint ucons))
  (let ((code (translate-blueprint blueprint)))
    (format t "~A~%" blueprint)
    (format t "~A~%" code)
    (finish-output)
    (compile nil code)))

(defmethod vm/execute ((virtual-machine common-lisp-virtual-machine) (kernel kernel))
  (funcall (vm/compile virtual-machine (kernel-blueprint kernel)) kernel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler "Gensyms"

(defun symbol-with-indices (name &rest indices)
  (format-symbol
   :petalisp/core/virtual-machines/common-lisp-virtual-machine
   "~A~{-~D~}" name indices))

(defun base-index-symbol (depth reference-id)
  (symbol-with-indices "BASE-INDEX" depth reference-id))

(defun index-symbol (n)
  (with-vector-memoization (n)
    (symbol-with-indices "INDEX" n)))

(defun array-symbol (n)
  (with-vector-memoization (n)
    (symbol-with-indices "ARRAY" n)))

(defun bound-symbol (n)
  (with-vector-memoization (n)
    (symbol-with-indices "BOUND" n)))

(defun accumulator-symbol (n)
  (with-vector-memoization (n)
    (symbol-with-indices "ACC" n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The kernel compiler

(defun translate-blueprint (blueprint)
  (declare (optimize debug))
  (ematch (ulist-deep-copy blueprint)
    ((list :blueprint bounds-metadata element-types body)
     (let* ((dimension (length bounds-metadata))
            (basic-blocks (make-array (1+ dimension) :initial-element nil))
            (n-reductions (count-reductions body))
            (block-id (- dimension n-reductions))
            (loop-variables (map 'vector #'index-symbol (iota dimension))))
       (declare (non-negative-fixnum block-id dimension)
                (simple-vector basic-blocks loop-variables))
       (labels
           ((translate (expression)
              (ematch expression
                ((list* :reference array-id indices)
                 (translate-reference array-id indices))
                ((list* :call operator expressions)
                 (apply #'translate-function-call operator
                        (mapcar #'translate expressions)))
                ((list :reduce binary-operator unary-operator expression)
                 (break "TODO"))
                ((list :store place expression)
                 (push
                  `(ignore (setf ,(translate place) ,(translate expression)))
                  (aref basic-blocks block-id))))))
         (translate body))
       (let ((optimized-blocks (optimize-subexpressions loop-variables basic-blocks)))
         `(lambda (kernel)
            (declare (kernel kernel)
                     (optimize debug))
            (let ((references (kernel-references kernel))
                  (unknown-functions (kernel-unknown-functions kernel))
                  (bounds (kernel-bounds kernel)))
              (declare (ignorable references unknown-functions bounds))
              (let ( ;; bind the storage arrays of each referenced immediate
                    ,@(loop for id from 0
                            for element-type in element-types
                            collect
                            `(,(array-symbol id)
                              (the (simple-array ,element-type)
                                   (storage (aref references ,id)))))
                    ;; bind the iteration space bounds of each dimension
                    ,@(loop for id below (length bounds-metadata)
                            for bounds-info in bounds-metadata
                            collect
                            `(,(bound-symbol id)
                              ,(if (integerp bounds-info)
                                   bounds-info
                                   `(the array-index (aref bounds ,id))))))
                ;; translate the basic blocks
                ,(translate-basic-blocks 0 optimized-blocks n-reductions)))))))))

(defun translate-basic-blocks (block-id blocks n-reductions)
  (unless (= block-id (length blocks))
    (let ((reduction? (>= block-id (- (length blocks) n-reductions)))
          (innermost? (= block-id (1- (length blocks)))))
      `(let* ,(aref blocks block-id)
         (declare (ignorable ,@(mapcar #'first (aref blocks block-id))))
         ,(cond (reduction? nil)
                (innermost? nil)
                (t `(loop for ,(index-symbol block-id) of-type fixnum
                     from 0 below ,(bound-symbol block-id) do
                     ,(translate-basic-blocks (1+ block-id) blocks n-reductions))))))))

(defun optimize-subexpressions (loop-variables basic-blocks)
  ;; BASIC-BLOCKS encodes the loop bodies of a kernel. Each basic block is
  ;; a list of variable declarations. This function traverses existing
  ;; bindings and hoists subexpressions where possible.
  (let ((result (make-array (length basic-blocks) :initial-element nil)))
    (labels
        ((bind (var expr depth)
           (prog1 var
             (push (list var expr)
                   (aref result depth))))
         (hoist (expression)
           (match expression
             ((and variable (type symbol))
              (let ((depth (1+ (or (position variable loop-variables) -1))))
                (values variable depth)))
             ((list 'setf (list* accessor arguments) value)
              (let ((max-depth 0))
                (flet ((hoist (expression)
                         (multiple-value-bind (result depth)
                             (hoist expression)
                             (prog1 result
                               (setf max-depth (max depth max-depth))))))
                  (values
                   `(setf (,accessor ,@(mapcar #'hoist arguments)) ,(hoist value))
                   max-depth))))
             ((list* function arguments)
              (let* ((max-depth 0)
                     (hoisted-expression
                       (list*
                        function
                        (loop for argument in arguments
                              collect
                              (multiple-value-bind (result depth)
                                  (hoist argument)
                                (prog1 result
                                  (setf max-depth (max depth max-depth))))))))
                (values
                 (or
                  ;; attempt CSE
                  (loop for (var expr) in (aref result max-depth)
                          thereis (and (equal expr hoisted-expression) var))
                  ;; otherwise add a new binding
                  (with-gensyms (tmp)
                    (bind tmp hoisted-expression max-depth)))
                 max-depth)))
             (constant (values constant 0)))))
      (declare (ftype (function (t) (values t non-negative-fixnum)) hoist))
      (loop for basic-block across basic-blocks
            for index from 0 do
              (loop for (var expression) in basic-block do
                (multiple-value-bind (result depth)
                    (hoist expression)
                  (bind var result depth))))
      (map-into result #'reverse result))))

(defun count-reductions (expression)
  (ematch expression
    ((list :reduce _ _ subexpression)
     (1+ (count-reductions subexpression)))
    ((list :store _ subexpression)
     (count-reductions subexpression))
    ((list* :reference _)
     0)
    ((list* :call _ subexpressions)
     (reduce #'+ subexpressions :key #'count-reductions))
    ((list :blueprint _ _ body)
     (count-reductions body))))

(defun translate-function-call (operator &rest arguments)
  (etypecase operator
    (symbol `(,operator ,@arguments))
    (integer `(funcall (aref unknown-functions ,operator) ,@arguments))))

(declaim (inline %fixnum-+ %fixnum-*))

(define-compiler-macro %fixnum-+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms collect
                   `(the fixnum ,form)))))

(define-compiler-macro %fixnum-* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms collect
                   `(the fixnum ,form)))))

(defun %fixnum-+ (&rest forms)
  (apply #'+ forms))

(defun %fixnum-* (&rest forms)
  (apply #'* forms))

(defun fixnum-+ (&rest forms)
  (match (remove 0 forms)
    ((list) 0)
    ((list form) form)
    ( list `(%fixnum-+ ,@list))))

(defun fixnum-* (&rest forms)
  (or (find 0 forms)
      (match (remove 1 forms)
        ((list) 1)
        ((list form) form)
        ( list `(%fixnum-* ,@list)))))

(defun translate-reference (array-id indices)
  (let ((quads (loop for |(id scale offset)| in indices
                     for dim from 0
                     collect (list* dim |(id scale offset)|))))
    (sort quads #'< :key #'first)
    (let ((index-form
            (apply #'fixnum-+
                   (loop for (dim id scale offset) in quads
                         collect
                         (fixnum-*
                          `(stride ,(array-symbol array-id) ,dim)
                          offset)))))
      (loop for (dim id scale offset) in quads do
        (setf index-form (fixnum-+
                          (fixnum-*
                           (index-symbol id)
                           (fixnum-* `(stride ,(array-symbol array-id) ,dim) scale))
                          index-form)))
      `(row-major-aref ,(array-symbol array-id) ,index-form))))

(defun stride (array axis)
  (declare (simple-array array)
           (array-index axis))
  (let ((stride 1))
    (declare (positive-fixnum stride))
    (loop for index of-type fixnum
          from (- (array-rank array) 2)
          downto axis do
            (setf stride (* (array-dimension array index) stride)))
    stride))
