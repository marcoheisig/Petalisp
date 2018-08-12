;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defclass common-lisp-backend
    (backend default-scheduler-mixin compile-cache-mixin)
  ((%memory-pool :reader memory-pool
                 :initform (make-hash-table :test #'equalp)
                 :type hash-table)
   (%worker-pool :reader worker-pool
                 :initform (lparallel:make-kernel 4))))

(defmethod vm/bind-memory
    ((backend common-lisp-backend)
     (immediate immediate))
  (let ((array-dimensions
          (map 'list #'set-size (ranges (shape immediate))))
        (element-type (element-type immediate)))
    (setf (storage immediate)
          (or
           (pop (gethash (cons element-type array-dimensions)
                         (memory-pool backend)))
           (make-array array-dimensions :element-type element-type)))
    (values)))

(defmethod vm/free-memory
    ((backend common-lisp-backend)
     (immediate immediate))
  (let ((array-dimensions
          (map 'list #'set-size (ranges (shape immediate))))
        (element-type (element-type immediate)))
    (push (storage immediate)
          (gethash (cons element-type array-dimensions)
                   (memory-pool backend)))
    (values)))

(defmethod vm/compile
  ((backend common-lisp-backend)
   (blueprint ucons:ucons))
  (let ((code (translate-blueprint blueprint)))
    (compile nil code)))

(defmethod vm/execute ((backend common-lisp-backend) (kernel kernel))
  (let ((lparallel:*kernel* (worker-pool backend)))
    (funcall (vm/compile backend (kernel-blueprint kernel)) kernel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler "Gensyms"

(defun symbol-with-indices (name &rest indices)
  (format-symbol *package* "~A~{-~D~}" name indices))

(defun base-index-symbol (depth reference-id)
  (symbol-with-indices "BASE-INDEX" depth reference-id))

(defun index-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "INDEX" n)))

(defun array-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "ARRAY" n)))

(defun bound-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "BOUND" n)))

(defun accumulator-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "ACC" n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with addresses and loops

(declaim (inline %emit-index-+ %fixnum-*))

(define-compiler-macro index-+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms collect
                   `(the fixnum ,form)))))

(define-compiler-macro index-* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms collect
                   `(the fixnum ,form)))))

(defun index-+ (&rest forms)
  (apply #'+ forms))

(defun index-* (&rest forms)
  (apply #'* forms))

(defun emit-index-+ (&rest forms)
  (trivia:match (remove 0 forms)
    ((list) 0)
    ((list form) form)
    ( list `(index-+ ,@list))))

(defun emit-index-* (&rest forms)
  (or (find 0 forms)
      (trivia:match (remove 1 forms)
        ((list) 1)
        ((list form) form)
        ( list `(index-* ,@list)))))

(defmacro for ((index start step end) &body body)
  (check-type index symbol)
  `(loop for ,index of-type fixnum
         from ,start by ,step below ,end
         do ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The kernel compiler

(defgeneric translate (statement))

(defstruct loop-statement
  (index nil)
  (start nil)
  (step nil)
  (end nil)
  (bindings nil)
  (body))

(defmethod translate ((loop loop-statement))
  `(let* ,(loop-statement-bindings loop)
     (for (,(loop-statement-index loop)
           ,(loop-statement-start loop)
           ,(loop-statement-step loop)
           ,(loop-statement-end loop))
       ,(translate (loop-statement-body loop)))))

(defstruct (parallel-loop-statement
            (:include loop-statement)))

(defmethod translate ((loop parallel-loop-statement))
  `(let* ,(loop-statement-bindings loop)
     (lparallel:pdotimes
         (,(loop-statement-index loop)
          ,(loop-statement-end loop))
       ,(translate (loop-statement-body loop)))))

(defstruct (reduction-statement
            (:include loop-statement))
  (binary-operator nil)
  (unary-operator nil))

(defmethod translate ((loop reduction-statement))
  (with-gensyms (eval-body acc)
    `(let* ,(loop-statement-bindings loop)
       (flet ((,eval-body (,(loop-statement-index loop))
                ,(translate (loop-statement-body loop))))
         (let ((,acc ,(translate-function-call
                       (reduction-statement-unary-operator loop)
                       `(,eval-body ,(loop-statement-start loop)))))
           (for (,(loop-statement-index loop)
                 (1+ ,(loop-statement-start loop))
                 ,(loop-statement-step loop)
                 ,(loop-statement-end loop))
                 (setf ,acc ,(translate-function-call
                              (reduction-statement-binary-operator loop)
                              acc
                              `(,eval-body ,(loop-statement-index loop)))))
           ,acc)))))

(defmethod translate ((s-expression list))
  (list* (first s-expression)
         (mapcar #'translate (rest s-expression))))

(defmethod translate ((symbol symbol)) symbol)

(defmethod translate ((integer integer)) integer)

(defun translate-blueprint (blueprint)
  (trivia:ematch (ucons:copy-utree blueprint)
    ((list :blueprint bounds-metadata element-types body)
     (let* ((dimension (length bounds-metadata))
            (loops (make-array dimension))
            (index (1- dimension))
            #+nil
            (min-size
              (loop for bound in bounds-metadata
                    counting
                    (ematch bound
                      ((list min _) min)
                      ((and bound (type integer)) bound)))))
       (labels
           ((walk (expression)
              (trivia:ematch expression
                ((list* :reference array-id indices)
                 (translate-reference array-id indices))
                ((list* :call operator expressions)
                 (apply #'translate-function-call operator
                        (mapcar #'walk expressions)))
                ((list :reduce binop unop expression)
                 (prog1 (setf (aref loops index)
                              (make-reduction-statement
                               :index (index-symbol index)
                               :start 0
                               :step 1
                               :end (bound-symbol index)
                               :binary-operator binop
                               :unary-operator unop
                               :body (walk expression)))
                   (decf index)))
                ((list :store place expression)
                 `(setf ,(walk place) ,(walk expression))))))
         (let ((body (walk body)))
           (loop for loop-index from index downto 0 do
                 #+nil
                  (if (and (= loop-index 0)
                           (> min-size 10000))
                      (setf body
                            (make-parallel-loop-statement
                             :index (index-symbol loop-index)
                             :start 0
                             :step 1
                             :end (bound-symbol loop-index)
                             :body body)))
                  (setf body
                        (make-loop-statement
                         :index (index-symbol loop-index)
                         :start 0
                         :step 1
                         :end (bound-symbol loop-index)
                         :body body))
                  (setf (aref loops loop-index) body))
           (optimize-loops loops)
           `(lambda (kernel)
              (declare (kernel kernel))
              ;;(with-unsafe-optimizations)
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
                  ,(translate body))))))))))

(defun optimize-loops (loops)
  (loop
    for loop across loops
    for index from 0 do
      (labels
          ((bind (var expr depth)
             (prog1 var
               (push (list var expr)
                     (loop-statement-bindings (aref loops depth)))))
           (hoist (expression)
             (trivia:match expression
               ((and variable (type symbol))
                (let ((depth (1+ (or (position variable loops
                                               :key #'loop-statement-index
                                               :test #'eq)
                                     -1))))
                  (values variable depth)))
               ((list 'setf (list* accessor arguments) value)
                (let ((max-depth 0))
                  (flet ((hoist (expression)
                           (multiple-value-bind (expr depth)
                               (hoist expression)
                             (prog1 expr
                               (setf max-depth (max depth max-depth))))))
                    (values
                     `(setf (,accessor ,@(mapcar #'hoist arguments)) ,(hoist value))
                     max-depth))))
               ((list* (and function (type symbol)) arguments)
                (let* ((max-depth 0)
                       (hoisted-expression
                         (list*
                          function
                          (loop for argument in arguments
                                collect
                                (multiple-value-bind (expr depth)
                                    (hoist argument)
                                  (prog1 expr
                                    (setf max-depth (max depth max-depth))))))))
                  (if (or (>= max-depth (length loops))
                          (> max-depth index))
                      (values hoisted-expression max-depth)
                      (values
                       (or
                        ;; attempt CSE
                        (loop for (var expr) in (loop-statement-bindings (aref loops max-depth))
                                thereis (and (equal expr hoisted-expression) var))
                        ;; otherwise add a new binding
                        (with-gensyms (tmp)
                          (bind tmp hoisted-expression max-depth)))
                       max-depth))))
               ((and constant (type fixnum))
                (values constant 0))
               (other (values other index)))))
        (declare (ftype (function (t) (values t non-negative-fixnum)) hoist))
        (setf (loop-statement-body loop)
              (hoist (loop-statement-body loop)))))
  (loop for loop across loops do
    (setf (loop-statement-bindings loop)
          (nreverse (loop-statement-bindings loop))))
  (values))

(defun list-of-reductions (expression)
  (trivia:ematch expression
    ((list :reduce binary-operator unary-operator subexpression)
     (cons (list binary-operator unary-operator)
           (list-of-reductions subexpression)))
    ((list :store _ subexpression)
     (list-of-reductions subexpression))
    ((list* :reference _) nil)
    ((list* :call _ subexpressions)
     (reduce #'append subexpressions :key #'list-of-reductions))
    ((list :blueprint _ _ body)
     (list-of-reductions body))))

(defun translate-function-call (operator &rest arguments)
  (etypecase operator
    (symbol `(,operator ,@arguments))
    (integer `(funcall (aref unknown-functions ,operator) ,@arguments))))

(defun translate-reference (array-id indices)
  (let ((quads (loop for |(id scale offset)| in indices
                     for dim from 0
                     collect (list* dim |(id scale offset)|))))
    (setf quads (sort quads #'< :key #'first))
    (let ((index-form
            (apply #'emit-index-+
                   (loop for (dim nil nil offset) in quads
                         collect
                         (emit-index-*
                          `(stride ,(array-symbol array-id) ,dim)
                          offset)))))
      (loop for (dim id scale nil) in quads do
        (setf index-form (emit-index-+
                          (emit-index-*
                           (index-symbol id)
                           (emit-index-* `(stride ,(array-symbol array-id) ,dim) scale))
                          index-form)))
      `(row-major-aref ,(array-symbol array-id) ,index-form))))

(defun stride (array axis)
  (declare (simple-array array)
           (array-index axis))
  (let ((stride 1))
    (declare (positive-fixnum stride))
    (loop for index of-type fixnum
          from (1- (array-rank array))
          downto (1+ axis) do
            (setf stride (* (array-dimension array index) stride)))
    stride))
