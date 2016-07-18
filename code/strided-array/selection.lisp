;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-selection (strided-array selection) ())

(defmethod selection ((object strided-array)
                      (space strided-array-index-space))
  (make-instance
   'strided-array-selection
   :object object
   :ranges (ranges space)
   :element-type (codomain-type object)))
