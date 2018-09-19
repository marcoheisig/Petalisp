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

(defclass native-backend-buffer (buffer)
  ((%transformation :initarg :transformation :reader transformation)
   (%storage :initarg :storage :accessor storage :initform nil)
   (%executedp :initarg :executedp :accessor executedp :initform nil)))

(defclass native-backend-kernel (kernel)
  ())

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
    :element-type (element-type strided-array)
    :transformation (collapsing-transformation (shape strided-array))))
