;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-transformation (strided-array transformation) ())

(defmethod generic-transform ((object strided-array)
                              (transformation affine-index-space-transformation))
  (make-instance
   'strided-array-transformation
   :object object
   :transformation transformation))
