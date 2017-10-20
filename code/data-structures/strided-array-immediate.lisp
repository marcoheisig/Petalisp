;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-immediate (strided-array immediate)
  ((storage      :type (or array null) :initform nil :accessor storage)
   (to-storage   :type transformation)
   (from-storage :type transformation)
   (dependencies :type vector :initform #())
   (kernels      :type vector :initform #())
   (users        :type vector :initform #())))

(defmethod petalispify ((array array))
  (let ((dimension (dimension array)))
    (make-instance 'strided-array-immediate
      :element-type (array-element-type array)
      :index-space (make-strided-array-index-space array)
      :storage array
      :to-storage (make-identity-transformation dimension)
      :from-storage (make-identity-transformation dimension))))

(defmethod depetalispify ((instance strided-array-immediate))
  (storage instance))

(defmethod optimize-application or ((f function) (a1 strided-array-immediate) &rest a2...aN)
  "Constant-fold operations on scalar values."
  (when (and (= 1 (size a1)) (every #'strided-array-immediate? a2...aN))
    (let ((value (apply f (row-major-aref (storage a1) 0)
                        (mapcar (λ ak (row-major-aref (storage ak) 0)) a2...aN))))
      (broadcast (petalispify value) (index-space a1)))))

(defmethod optimize-application or ((f function) (a1 strided-array-reference) &rest a2...aN)
  "Constant-fold operations on references to scalar values."
  (flet ((scalar-reference? (a)
           (when (strided-array-reference? a)
             (let ((input (input a)))
               (and (strided-array-immediate? input)
                    (= 1 (size input)))))))
    (when (and (scalar-reference? a1) (every #'scalar-reference? a2...aN))
      (let ((value (apply f (row-major-aref (storage (input a1)) 0)
                          (mapcar (λ ak (row-major-aref (storage (input ak)) 0)) a2...aN))))
        (reference (petalispify value) (index-space a1) (transformation a1))))))
