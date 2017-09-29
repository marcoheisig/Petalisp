;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-immediate (strided-array immediate) ())

(define-class strided-array-constant (strided-array-immediate)
  ((storage :type (or array nil) :initform nil :accessor storage)
   (transformation :type transformation)))

(defmethod petalispify ((array array))
  (make-instance 'strided-array-constant
    :element-type (array-element-type array)
    :storage array
    :index-space (make-strided-array-index-space array)
    :transformation (make-identity-transformation (dimension array))))

(defmethod depetalispify ((instance strided-array-constant))
  (storage instance))

(defmethod print-object ((object strided-array-constant) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (index-space object) stream)
    (write-char #\space stream)
    (princ (transformation object) stream)))

(defmethod optimize-reference or ((object strided-array-constant)
                                  (space strided-array-index-space)
                                  (transformation transformation))
  (make-instance 'strided-array-constant
    :storage (storage object)
    :element-type (element-type object)
    :index-space space
    :transformation (composition (transformation object) transformation)))

(defmethod optimize-application or ((f function) (a1 strided-array-constant) &rest a2...aN)
  "Constant-fold operations on scalar values."
  (when (and (= 1 (size a1)) (every #'strided-array-constant? a2...aN))
    (let ((value (apply f (row-major-aref (storage a1) 0)
                        (mapcar (λ ak (row-major-aref (storage ak) 0)) a2...aN))))
      (broadcast (petalispify value) (index-space a1)))))
