;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;; The purpose of the IR backend is to check that the IR conversion
;;; preserves semantics.  It is similar to the reference backend, but
;;; evaluates kernels instead of individual strided arrays.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric execute (ir-node))

(defgeneric immediate-from-buffer (buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass petalisp:ir-backend (backend)
  ())

(defclass ir-backend-buffer (buffer)
  ((%transformation :initarg :transformation :reader transformation)
   (%storage :initarg :storage :reader storage)
   (%executedp :initarg :executedp :accessor executedp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-immediates ((strided-arrays list) (ir-backend ir-backend))
  (mapcar (compose #'immediate-from-buffer #'evaluate)
          (ir-from-strided-arrays strided-arrays ir-backend)))

(defmethod immediate-from-buffer ((ir-backend-buffer ir-backend-buffer))
  (make-array-immediate (storage ir-backend-buffer)))

(defmethod make-buffer ((strided-array strided-array) (ir-backend ir-backend))
  (make-instance 'ir-backend-buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)
    :transformation (collapsing-transformation (shape strided-array))
    :storage (make-array (mapcar #'set-size (ranges (shape strided-array))))
    :executedp nil))

(defmethod make-buffer ((array-immediate array-immediate) (ir-backend ir-backend))
  (make-instance 'ir-backend-buffer
    :shape (shape array-immediate)
    :element-type (element-type array-immediate)
    :transformation (make-identity-transformation (dimension array-immediate))
    :storage (storage array-immediate)
    :executedp t))

(defmethod make-buffer ((scalar-immediate scalar-immediate) (ir-backend ir-backend))
  (make-instance 'ir-backend-buffer
    :shape (shape scalar-immediate)
    :element-type (element-type scalar-immediate)
    :transformation (make-identity-transformation 0)
    :storage (make-array '() :initial-element (storage scalar-immediate))
    :executedp t))

(defmethod make-buffer ((range-immediate range-immediate) (ir-backend ir-backend))
  (let ((transformation (collapsing-transformation (shape range-immediate)))
        (axis (axis range-immediate)))
    (make-instance 'ir-backend-buffer
      :shape (shape range-immediate)
      :element-type (element-type range-immediate)
      :transformation transformation
      :storage
      (let ((storage (make-array (mapcar #'set-size (ranges (shape range-immediate))))))
        (loop for index in (set-elements (shape range-immediate)) do
          (setf (apply #'aref storage (transform index transformation))
                (nth axis index)))
        storage)
      :executedp t)))

(defmethod execute :before ((ir-node ir-node))
  (mapc #'execute (inputs ir-node)))

(defmethod execute :around ((ir-backend-buffer ir-backend-buffer))
  (unless (executedp ir-backend-buffer)
    (call-next-method)))

(defmethod execute ((ir-backend-buffer ir-backend-buffer))
  (mapc #'execute (inputs ir-backend-buffer)))

(defmethod execute ((kernel kernel))
  (funcall (compile-kernel kernel)))
