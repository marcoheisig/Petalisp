;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(define-class identity-transformation (transformation)
  ((input-dimension :type (integer 0 *)))
  (:metaclass funcallable-standard-class))

(defun identity-transformation (dimension)
  (with-vector-memoization (dimension)
    (make-instance 'identity-transformation :input-dimension dimension)))

(defmethod composition ((g identity-transformation) (f transformation)) f)

(defmethod composition ((g transformation) (f identity-transformation)) g)

(defmethod enlarge-transformation ((transformation identity-transformation))
  (identity-transformation (1+ (dimension transformation))))

(defmethod equal? ((a identity-transformation) (b identity-transformation))
  (= (dimension a) (dimension b)))

(defmethod generic-unary-funcall ((operator identity-transformation)
                                  (argument t))
  argument)

(defmethod inverse ((transformation identity-transformation)) transformation)

(defmethod output-dimension ((I identity-transformation)) (input-dimension I))

(define-symbol-pool index-symbol "I")

(defmethod print-object ((object identity-transformation) stream)
  (let ((index-symbols (index-symbol-list (input-dimension object))))
    (prin1 `(τ ,index-symbols ,index-symbols) stream)))

(defmethod reference or ((object data-structure)
                         (space index-space)
                         (transformation identity-transformation))
  "Drop references with no effect."
  (when (equal? (index-space object) space) object))
