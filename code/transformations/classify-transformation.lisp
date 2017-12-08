;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(defun classify-transformation (f &optional input-constraints)
  (let* ((input-dimension (function-arity f))
         (input-constraints
           (if input-constraints
               (prog1 input-constraints
                 (assert (= (length input-constraints) input-dimension)))
               (make-array input-dimension :initial-element nil))))
    (let ((args (map 'list (λ constraint (or constraint 0)) input-constraints))
          ;; F is applied to many slightly different arguments, so we build a
          ;; vector pointing to the individual conses of ARGS for fast random
          ;; access.
          (arg-conses (make-array input-dimension)))
      (iterate (for arg-cons on args)
               (for i from 0)
               (setf (aref arg-conses i) arg-cons))
      ;; Initially x is the zero vector (except for input constraints, which
      ;; are ignored by A), so f(x) = Ax + b = b
      (let* ((translation-vector (multiple-value-call #'vector (apply f args)))
             (output-dimension (length translation-vector))
             ;; now determine the scaled permutation matrix A
             (column-indices (make-array output-dimension
                                         :element-type 'array-index
                                         :initial-element 0))
             (values (make-array output-dimension
                                 :element-type 'rational
                                 :initial-element 0)))
        ;; set one input at a time from zero to one (ignoring those with
        ;; constraints) and check how it changes the result
        (iterate (for input-constraint in-vector input-constraints)
                 (for arg-cons in-vector arg-conses)
                 (for column-index from 0)
                 (unless input-constraint
                   (setf (car arg-cons) 1)
                   ;; find the row of A corresponding to the mutated input
                   (let ((results (multiple-value-call #'vector (apply f args))))
                     (iterate (for result in-vector results)
                              (for offset in-vector translation-vector)
                              (for row-index from 0)
                              (when (/= result offset)
                                (setf (aref column-indices row-index) column-index)
                                (setf (aref values row-index) (- result offset)))))
                   (setf (car arg-cons) 0)))
        (let ((linear-operator
                (scaled-permutation-matrix
                 output-dimension input-dimension column-indices values)))
          ;; optional but oh so helpful: check whether the derived mapping
          ;; satisfies other inputs, i.e. the mapping can indeed be represented
          ;; as Ax + b
          (iterate (for input-constraint in-vector input-constraints)
                   (for arg-cons in-vector arg-conses)
                   (unless input-constraint (setf (car arg-cons) 3)))
          (let* ((result-1 (multiple-value-call #'vector (apply f args)))
                 (Ax (matrix-product
                      linear-operator
                      (make-array input-dimension
                                  :element-type 'rational
                                  :initial-contents args)))
                 (result-2 (map 'vector #'+ Ax translation-vector)))
            (assert (every #'= result-1 result-2) ()
                    "Not a valid transformation:~%  ~S"
                    f))
          (affine-transformation
           input-constraints
           linear-operator
           translation-vector))))))

;; It is an error to classify a function that is not referentially
;; transparent. Furthermore transformations are immutable. As a result, the
;; classification of functions without free variables can be preponed to
;; load time.
(define-compiler-macro classify-transformation
    (&whole whole &environment env function &optional input-constraints)
  (if (and (constantp input-constraints env)
           (not (free-variables function)))
      `(load-time-value
        (locally
            (declare (notinline classify-transformation))
          (classify-transformation
           ,function
           ,input-constraints)))
      whole))

(defmacro τ (input-forms output-forms)
  (let* ((input-dimension (length input-forms))
         (input-constraints (make-array input-dimension :initial-element nil)))
    (iterate (for form in input-forms)
             (for index from 0)
             (cond ((integerp form)
                    (setf (aref input-constraints index) form)
                    (collect (gensym) into symbols))
                   ((symbolp form)
                    (collect form into symbols))
                   (t
                    (simple-program-error
                     "Invalid transformation lambda list item: ~S" form)))
             (finally
              (return
                `(classify-transformation
                  (lambda ,symbols
                    (declare (ignorable ,@symbols))
                    (values ,@output-forms))
                  ,input-constraints))))))
