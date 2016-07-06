;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-reduction (strided-array reduction) ())

(defmethod generic-reduce ((operator operator) (object strided-array))
  (let ((ranges (nreverse (cdr (reverse (ranges object)))))
        (element-type (element-type argument)))
    (assert (petalisp-subtypep
             (result-type operator element-type element-type)
             element-type))
    (make-instance
     'strided-array-reduction
     :operator operator
     :object object
     :ranges ranges
     :element-type element-type)))
