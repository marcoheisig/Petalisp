;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-immediate (strided-array immediate)
  ((storage :type array)
   (transformation :type transformation)))

(defmethod petalispify ((array array))
  (make-instance 'strided-array-immediate
    :element-type (array-element-type array)
    :storage array
    :index-space (make-strided-array-index-space array)
    :transformation (make-identity-transformation (dimension array))))

(defmethod depetalispify ((instance strided-array-immediate))
  (storage instance))

(defmethod optimize-reference or ((object strided-array-immediate)
                                  (space strided-array-index-space)
                                  (transformation transformation))
  (make-instance 'strided-array-immediate
    :storage (storage object)
    :element-type (element-type object)
    :index-space space
    :transformation (composition (transformation object) transformation)))

(defmethod optimize-application or ((f function) (a1 strided-array-immediate) &rest a2...aN)
  "Constant-fold operations on scalar values."
  (when (and (= 1 (size a1)) (every #'strided-array-immediate? a2...aN))
    (petalispify (apply f (row-major-aref (storage a1) 0)
                        (mapcar (λ ak (row-major-aref (storage ak) 0)) a2...aN)))))
