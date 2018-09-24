;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric immediate-from-buffer (buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass native-backend-immediate (buffer)
  ())

(defclass native-backend-array-immediate (native-backend-immediate)
  ((%storage :initarg :storage :reader storage)))

(defclass native-backend-scalar-immediate (native-backend-immediate)
  ((%storage :initarg :storage :reader storage)))

(defclass native-backend-range-immediate (native-backend-immediate)
  ((%axis :initarg :axis :reader axis)))

(defclass native-backend-buffer (petalisp-ir:buffer)
  ((%transformation :initarg :transformation :accessor transformation)
   (%storage :initarg :storage :accessor storage)))

(defclass native-backend-kernel (petalisp-ir:kernel)
  ((%executedp :initarg :executedp :accessor executedp
               :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod immediate-from-buffer
    ((native-backend-buffer native-backend-buffer))
  (make-array-immediate (storage native-backend-buffer)))

(defmethod make-buffer ((array-immediate array-immediate)
                        (native-backend native-backend))
  (make-instance 'native-backend-array-immediate
    :shape (shape array-immediate)
    :element-type (element-type array-immediate)
    :storage (storage array-immediate)))

(defmethod make-buffer ((scalar-immediate scalar-immediate) (native-backend native-backend))
  (make-instance 'native-backend-scalar-immediate
    :shape (shape scalar-immediate)
    :element-type (element-type scalar-immediate)
    :storage (storage scalar-immediate)))

(defmethod make-buffer ((range-immediate range-immediate)
                        (native-backend native-backend))
  (make-instance 'native-backend-range-immediate
    :shape (shape range-immediate)
    :element-type (element-type range-immediate)
    :axis (axis range-immediate)))

(defmethod make-buffer ((strided-array strided-array)
                        (native-backend native-backend))
  (make-instance 'native-backend-buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)))

(defmethod make-kernel ((iteration-space shape)
                        (body list)
                        (outputs list)
                        (inputs list)
                        (backend native-backend))
  (make-instance 'native-backend-kernel
    :shape iteration-space
    :inputs inputs
    :outputs outputs
    :body body))

(defmethod transformation :before ((native-backend-buffer native-backend-buffer))
  (unless (slot-boundp native-backend-buffer '%transformation)
    (setf (slot-value native-backend-buffer '%transformation)
          (collapsing-transformation (shape native-backend-buffer)))))
