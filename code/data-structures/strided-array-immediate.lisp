;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-immediate (strided-array immediate) ())

(define-class strided-array-constant (strided-array-immediate)
  ((storage :type array :initform nil :accessor storage)))

(defmethod petalispify ((array array))
  (make-instance 'strided-array-constant
    :element-type (array-element-type array)
    :storage array
    :index-space (make-strided-array-index-space array)))

(defmethod depetalispify ((instance strided-array-constant))
  (storage instance))

(defmethod optimize-application or ((f function) (a1 strided-array-constant) &rest a2...aN)
  "Constant-fold operations on scalar values."
  (when (and (= 1 (size a1)) (every #'strided-array-constant? a2...aN))
    (let ((value (apply f (row-major-aref (storage a1) 0)
                        (mapcar (λ ak (row-major-aref (storage ak) 0)) a2...aN))))
      (broadcast (petalispify value) (index-space a1)))))

(defmethod optimize-application or ((f function) (a1 strided-array-reference) &rest a2...aN)
  "Constant-fold operations on references to scalar values."
  (flet ((scalar-reference? (a)
           (when (strided-array-reference? a)
             (let ((input (input a)))
               (and (strided-array-constant? input)
                    (= 1 (size input)))))))
    (when (and (scalar-reference? a1) (every #'scalar-reference? a2...aN))
      (let ((value (apply f (row-major-aref (storage (input a1)) 0)
                          (mapcar (λ ak (row-major-aref (storage (input ak)) 0)) a2...aN))))
        (reference (petalispify value) (index-space a1) (transformation a1))))))
