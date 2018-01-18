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
  (let ((code (translate (ulist-deep-copy blueprint) 0)))
    ;(format t "~A~%" blueprint)
    ;(format t "~A~%" code)
    ;(finish-output)
    (compile nil code)))

(defmethod vm/execute ((virtual-machine common-lisp-virtual-machine) (kernel kernel))
  (funcall (vm/compile virtual-machine (kernel-blueprint kernel)) kernel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The kernel compiler

(defun symbol-with-indices (name &rest indices)
  (format-symbol
   :petalisp/core/virtual-machines/common-lisp-virtual-machine
   "~A~{~D~^-~}" name indices))

(defun stride-symbol (array-id depth)
  (symbol-with-indices "STRIDE" array-id depth))

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

(defun translate (expression depth)
  (ematch expression
    ((list :blueprint bounds-metadata element-types body)
     (let ((outer-dimension (- (length bounds-metadata)
                               (count-reductions body))))
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
              ;; translate the body
              (with-loops (0 ,outer-dimension)
                ,(translate body outer-dimension)))))))
    ((list* :call operator expressions)
     (apply #'translate-function-call operator
            (flet ((translate-expression (input)
                     (translate input depth)))
              (mapcar #'translate-expression expressions))))
    ((list* :reference array-id indices)
     `(aref
       ,(array-symbol array-id)
       ,@(flet ((translate-index (index-triple)
                  (destructuring-bind (scale offset id) index-triple
                    (symbolic-+
                     offset
                     (symbolic-* scale (index-symbol id))))))
           (mapcar #'translate-index indices))))
    ((list :reduce binary-operator unary-operator expression)
     (let ((body (translate expression (1+ depth))))
       `(loop with accumulator
                = (let ((,(index-symbol depth) 0))
                    (declare (ignorable ,(index-symbol depth)))
                    ,(translate-function-call unary-operator body))
              for ,(index-symbol depth) of-type fixnum from 1 below ,(bound-symbol depth)
              do (setf accumulator
                       ,(translate-function-call binary-operator 'accumulator body))
              finally (return accumulator))))
    ((list :store place expression)
     `(setf ,(translate place depth) ,(translate expression depth)))))

(defmacro with-loops ((current total) &body body)
  (if (= current total)
      `(progn ,@body)
      `(dotimes (,(index-symbol current) ,(bound-symbol current))
         (with-loops (,(1+ current) ,total)
           ,@body))))

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
