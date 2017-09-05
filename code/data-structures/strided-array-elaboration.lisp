;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-elaboration (strided-array elaboration)
  ((data :type array)))

(defmethod petalispify ((array array))
  (make-instance
   'strided-array-elaboration
   :element-type (array-element-type array)
   :data array
   :index-space (make-strided-array-index-space array)))

(defmethod depetalispify ((instance strided-array-elaboration))
  (data instance))
