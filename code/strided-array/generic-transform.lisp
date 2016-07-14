;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-affine-permutation (strided-array affine-permutation)
  ((%coefficients :initarg :coefficients :reader coefficients)))

(defmethod generic-transform ((object strided-array)
                              &key scale translate permute)
  (make-instance
   'strided-array-transformation
   :object object
   :coefficients ()))
