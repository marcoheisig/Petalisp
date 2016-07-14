;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-index-space ((object strided-array))
  (slot-value object '%index-space))

(defmethod initialize-instance :after ((object strided-array)
                                       &key &allow-other-keys)
  (setf (slot-value object '%index-space)
        (make-instance
         'strided-array-index-space
         :ranges (ranges object))))

(defmethod initialize-instance :after ((object strided-array-index-space)
                                       &key &allow-other-keys)
  (setf (slot-value object '%index-space)
        object))
