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

(define-class identity-transformation (transformation)
  ((input-dimension :type (integer 0 *)))
  (:metaclass funcallable-standard-class))

(defun identity-transformation (dimension)
  (with-vector-memoization (dimension)
    (make-instance 'identity-transformation :input-dimension dimension)))

(defmethod composition ((g identity-transformation) (f transformation)) f)

(defmethod composition ((g transformation) (f identity-transformation)) g)

(defmethod enlarge-transformation ((transformation identity-transformation))
  (identity-transformation (1+ (input-dimension transformation))))

(defmethod generic-unary-funcall ((operator identity-transformation)
                                  (argument t))
  argument)

(defmethod inverse ((transformation identity-transformation)) transformation)

(defmethod map-transformation-into ((transformation identity-transformation)
                                    (result-sequence sequence)
                                    (function function)
                                    &rest sequences)
  (flet ((process-one-output (&rest inputs)
           (declare (dynamic-extent inputs))
           (apply function 1 0 inputs)))
    (apply #'map-into result-sequence #'process-one-output sequences)))

(defmethod output-dimension ((I identity-transformation)) (input-dimension I))

(defmethod print-object ((object identity-transformation) stream)
  (let ((indices (iota (input-dimension object))))
    (format stream "~:<τ~2I ~:<~{i~D~^ ~:_~}~:> ~_~:<~{i~D~^ ~:_~}~:>~:>"
            (list (list indices) (list indices)))))

