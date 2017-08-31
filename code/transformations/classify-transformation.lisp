;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod classify-transformation ((f function)
                                    (input-constraints vector)
                                    (output-dimension integer))
  (let* ((input-dimension (length input-constraints))
         (args (map 'list
                    (λ constraint (or constraint 0))
                    input-constraints))
         ;; F is applied to many slightly different arguments, so we build a
         ;; vector pointing to the individual conses of ARGS for fast random
         ;; access.
         (arg-conses (make-array input-dimension)))
    (loop :for arg-cons :on args :and i :from 0 :do
      (setf (aref arg-conses i) arg-cons))
    ;; Initially x is the zero vector (except for input constraints, which
    ;; are ignored by A), so f(x) = Ax + b = b
    (let ((translation-vector (multiple-value-call #'vector (apply f args)))
          ;; now determine the super sparse matrix A
          (column-indices (make-array output-dimension
                                      :element-type 'array-index
                                      :initial-element 0))
          (values (make-array output-dimension
                              :element-type 'rational
                              :initial-element 0)))
      ;; set one input at a time from zero to one (ignoring those with
      ;; constraints) and check how it changes the result
      (loop :for input-constraint :across input-constraints
            :for arg-cons :across arg-conses
            :for column-index :from 0 :do
              (unless input-constraint
                (setf (car arg-cons) 1)
                ;; find the row of A corresponding to the mutated input
                (let ((results (multiple-value-call #'vector (apply f args))))
                  (loop :for result :across results
                        :for offset :across translation-vector
                        :for row-index :from 0 :do
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
        (loop :for input-constraint :across input-constraints
              :for arg-cons :across arg-conses :do
                (unless input-constraint
                  (setf (car arg-cons) 3)))
        (let* ((result-1 (multiple-value-call #'vector args))
               (Ax (product
                    linear-operator
                    (make-array input-dimension
                                :element-type 'rational
                                :initial-contents args)))
               (result-2 (map 'vector #'- Ax translation-vector)))
          (assert (every #'= result-1 result-2) ()
                  "Not a valid transformation:~%  ~S"
                  f))
        (if (and (= input-dimension output-dimension)
                 (every #'null input-constraints)
                 (every #'zerop translation-vector)
                 (matrix-identity? linear-operator))
            (make-instance
             'identity-transformation
             :dimension input-dimension)
            (make-instance
             'affine-trainsformation
             :input-constraints input-constraints
             :linear-operator linear-operator
             :translation-vector translation-vector))))))

(defmacro τ (input-forms &rest output-forms)
  (loop for form in input-forms
        collect (when (integerp form) form) into input-constraints
        collect (if (symbolp form) form (gensym)) into symbols
        finally
           (return
             `(classify-transformation
               (lambda ,symbols
                 (declare (ignorable ,@symbols))
                 (values ,@output-forms))
               ,(apply #'vector input-constraints)
               ,(length output-forms)))))
