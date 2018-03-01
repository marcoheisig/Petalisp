;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/identity-transformation
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/transformation)
  (:export
   #:identity-transformation
   #:identity-transformation?))

(in-package :petalisp/core/transformations/identity-transformation)

(defclass identity-transformation (transformation)
  ((%input-dimension :initarg :input-dimension
                     :reader input-dimension
                     :type (integer 0 *)))
  (:metaclass funcallable-standard-class))

(defun identity-transformation (dimension)
  (with-vector-memoization (dimension)
    (make-instance 'identity-transformation :input-dimension dimension)))

(defmethod composition ((g identity-transformation) (f transformation)) f)

(defmethod composition ((g transformation) (f identity-transformation)) g)

(defmethod enlarge-transformation ((transformation identity-transformation) scale offset)
  (identity-transformation (1+ (input-dimension transformation))))

(defmethod generic-unary-funcall ((operator identity-transformation)
                                  (argument t))
  argument)

(defmethod inverse ((transformation identity-transformation)) transformation)

(defmethod do-outputs ((transformation identity-transformation)
                       (function function)
                       &rest sequences)
  (let ((args (make-list (length sequences))))
    (declare (list args))
    (flet ((set-args! (index)
             (loop for arg-cons on args
                   and sequence in sequences do
                     (setf (car arg-cons) (elt sequence index)))))
      (loop for index below (input-dimension transformation) do
        (set-args! index)
        (apply function index index 1 0 args)))))

(defmethod output-dimension ((I identity-transformation)) (input-dimension I))

(defmethod print-object ((object identity-transformation) stream)
  (let ((indices (iota (input-dimension object))))
    (format stream "~:<τ~2I ~:<~{i~D~^ ~:_~}~:> ~_~:<~{i~D~^ ~:_~}~:>~:>"
            (list (list indices) (list indices)))))

