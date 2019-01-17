;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric bref (buffer index))

(defgeneric (setf bref) (value buffer index))

(defgeneric immediate-from-buffer (buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-node ()
  ((%executedp :initarg :executedp :accessor executedp)))

(defclass kernel (petalisp.ir:kernel ir-node)
  ())

(defclass buffer (petalisp.ir:buffer ir-node)
  ((%storage :initarg :storage :reader storage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Conversion

(defmethod petalisp.ir:make-kernel ((ir-backend ir-backend) &rest args)
  (apply #'make-instance 'kernel :executedp nil args))

(defmethod petalisp.ir:make-buffer
    ((strided-array strided-array) (ir-backend ir-backend))
  (make-instance 'buffer
    :executedp nil
    :shape (shape strided-array)
    :element-type (element-type strided-array)
    :storage (make-array (mapcar #'set-size (ranges (shape strided-array))))))

(defmethod petalisp.ir:make-buffer
    ((array-immediate array-immediate) (ir-backend ir-backend))
  (make-instance 'buffer
    :executedp t
    :shape (shape array-immediate)
    :element-type (element-type array-immediate)
    :storage (storage array-immediate)))

(defmethod petalisp.ir:make-buffer
    ((range-immediate range-immediate) (ir-backend ir-backend))
  (let* ((size (set-size (shape range-immediate)))
         (element-type (element-type range-immediate))
         (array (make-array size :element-type element-type)))
    (loop for index below size do
      (setf (aref array index) index))
    (make-instance 'buffer
      :executedp t
      :shape (shape range-immediate)
      :element-type (element-type range-immediate)
      :storage array)))

(defmethod immediate-from-buffer ((buffer buffer))
  (make-instance 'array-immediate
    :element-type (element-type buffer)
    :shape (petalisp.ir:buffer-shape buffer)
    :storage (storage buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer References

(defmethod bref ((buffer buffer) (index list))
  (apply #'aref (storage buffer) index))

(defmethod (setf bref) (value (buffer buffer) index)
  (setf (apply #'aref (storage buffer) index) value))
