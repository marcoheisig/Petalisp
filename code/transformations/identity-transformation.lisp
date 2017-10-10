;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class identity-transformation (transformation)
  ((input-dimension :type (integer 0 *)))
  (:metaclass funcallable-standard-class))

(declaim (inline make-identity-transformation))
(defun make-identity-transformation (dimension)
  (let ((memoization-table
          (load-time-value
           (map 'vector
                (λ dimension
                   (make-instance 'identity-transformation
                     :input-dimension dimension))
                (iota 20))
           t)))
    (or (and (< dimension (length memoization-table))
             (aref memoization-table dimension))
        (make-instance 'identity-transformation :input-dimension dimension))))

(defmethod composition ((g identity-transformation) (f transformation)) f)

(defmethod composition ((g transformation) (f identity-transformation)) g)

(defmethod equal? ((a identity-transformation) (b identity-transformation))
  (= (dimension a) (dimension b)))

(defmethod generic-unary-funcall ((operator identity-transformation)
                                  (argument index-space))
  (assert (= (input-dimension operator) (dimension argument)))
  argument)

(defmethod inverse ((transformation identity-transformation)) transformation)

(defmethod output-dimension ((I identity-transformation)) (input-dimension I))

(defmethod print-object ((object identity-transformation) stream)
  (let ((symbols (list-of-symbols (input-dimension object))))
    (prin1 `(τ ,symbols ,@symbols) stream)))

(defmethod optimize-reference or ((object data-structure)
                                  (space index-space)
                                  (transformation identity-transformation))
  "Drop references with no effect."
  (when (equal? (index-space object) space) object))
