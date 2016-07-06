;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-index-space ((object strided-array-index-space))
  object)

(defmethod generic-index-space ((object strided-array))
  (make-instance
   'strided-array-index-space
   :ranges (ranges object)
   :value-type 'list))
