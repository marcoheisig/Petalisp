;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reference (strided-array reference)
  (source-space))

(defmethod reference ((object strided-array)
                      (source-space strided-array-index-space)
                      &optional transformation)
  (let ((ranges (ranges (transform source-space transformation))))
    (make-instance
     'strided-array-reference
     :object object
     :ranges ranges
     :source-space source-space
     :transformation transformation)))
