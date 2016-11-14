;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reference (strided-array reference)
  (source-space))

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      transformation)
  (let ((target-space (transform space transformation)))
    (if (equal? (index-space object) target-space)
        object
        (make-instance
         'strided-array-reference
         :predecessors (list object)
         :ranges (ranges target-space)
         :element-type (element-type object)
         :source-space space
         :transformation transformation))))
