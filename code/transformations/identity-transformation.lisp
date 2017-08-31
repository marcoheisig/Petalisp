;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class identity-transformation (transformation)
  ((input-dimension :type (integer 0 *)))
  (:metaclass funcallable-standard-class))

(defmethod composition ((g identity-transformation) (f transformation)) f)

(defmethod composition ((g transformation) (f identity-transformation)) g)

(defmethod equal? ((a identity-transformation) (b identity-transformation))
  (= (dimension a) (dimension b)))

(defmethod generic-unary-funcall ((operator identity-transformation)
                                  (argument data-structure))
  (assert (= (input-dimension operator) (dimension argument)))
  argument)

(defmethod inverse ((transformation identity-transformation)) transformation)

(defmethod output-dimension ((I identity-transformation)) (input-dimension I))

(defmethod print-object ((object identity-transformation) stream)
  (let ((symbols (list-of-symbols (input-dimension object))))
    (prin1 `(τ ,symbols ,@symbols))))
