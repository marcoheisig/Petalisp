;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun make-affine-transformation (input-constraints A b)
  (declare (type scaled-permutation-matrix A)
           (type simple-vector input-constraints b))
  (if (and (= (length input-constraints) (matrix-n A) (matrix-m A))
           (every #'null input-constraints)
           (every #'zerop b)
           (identity-matrix? A))
      (make-identity-transformation (length input-constraints))
      (make-instance
       'affine-transformation
       :input-constraints input-constraints
       :linear-operator A
       :translation-vector b)))

(defun classify-transformation (f input-constraints output-dimension)
  (let* ((input-dimension (length input-constraints))
         (args (map 'list
                    (λ constraint (or constraint 0))
                    input-constraints))
         ;; F is applied to many slightly different arguments, so we build a
         ;; vector pointing to the individual conses of ARGS for fast random
         ;; access.
         (arg-conses (make-array input-dimension)))
    (iterate (for arg-cons on args)
             (for i from 0)
             (setf (aref arg-conses i) arg-cons))
    ;; Initially x is the zero vector (except for input constraints, which
    ;; are ignored by A), so f(x) = Ax + b = b
    (let ((translation-vector (multiple-value-call #'vector (apply f args)))
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
        (make-affine-transformation
         input-constraints
         linear-operator
         translation-vector)))))

(defmacro τ (input-forms &rest output-forms)
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
                  ,input-constraints
                  ,(length output-forms)))))))

(test identity-transformation
  (let ((τ (τ (a b) a b)))
    (is (identity-transformation? τ))
    (is (= 2 (input-dimension τ) (output-dimension τ))))
  (let ((τ (τ (a b c d) a b (ash (* 2 c) -1) (+ d 0))))
    (is (identity-transformation? τ))
    (is (= 4 (input-dimension τ) (output-dimension τ))))
  (for-all ((dimension (integer-generator 0 200)))
    (let ((τ (make-identity-transformation dimension)))
      (is (identity-transformation? τ))
      (is (equal? τ τ))
      (is (equal? τ (inverse τ)))
      (is (equal? τ (composition τ τ))))))

(test affine-transformation
  (dolist (τ (list (τ (m n) n m)
                   (τ (m) (* 2 m))
                   (τ (m) (/ (+ (* 90 (+ 2 m)) 15) 2))
                   (τ (m) m 1 2 3)
                   (τ (m) 5 9 m 2)
                   (τ (0 n 0) n)
                   (τ (i j 5) i j)))
    (let ((τ-inv (inverse τ)))
      (is (affine-transformation? τ))
      (is (equal? τ (inverse τ-inv)))
      (when (and (every #'null (input-constraints τ))
                 (every #'null (input-constraints τ-inv)))
        (is (equal? (composition τ-inv τ)
                    (make-identity-transformation
                     (input-dimension τ))))))))
