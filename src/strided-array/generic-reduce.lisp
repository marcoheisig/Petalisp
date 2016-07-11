;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-reduction (strided-array reduction) ())

(defmethod generic-reduce ((operator total-function) (object strided-array))
  (let ((ranges (nreverse (cdr (reverse (ranges object)))))
        (codomain-type (codomain-type argument)))
    (assert (petalisp-subtypep
             (result-type operator element-type element-type)
             element-type))
    (make-instance
     'strided-array-reduction
     :operator operator
     :object object
     :ranges ranges
     :codomain-type element-type)))
