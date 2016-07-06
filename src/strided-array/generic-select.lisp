;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-selection (strided-array selection) ())

(defmethod generic-select ((object strided-array)
                           (space strided-array-index-space))
  (let ((intersection (index-space-intersection
                       index-space
                       (index-space object))))
    (assert (and intersection
                 (index-space= intersection index-space))))
  (make-instance
   'strided-array-selection
   :object object
   :index-space index-space
   :element-type (element-type object)))
