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

(defclass ir-backend (petalisp:backend)
  ())

(defclass ir-backend-buffer (petalisp-ir:buffer)
  ((%transformation :initarg :transformation :reader transformation)
   (%storage :initarg :storage :reader storage)
   (%executedp :initarg :executedp :accessor executedp)))

(defun make-ir-backend ()
  (make-instance 'ir-backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-immediates ((strided-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (petalisp-ir:ir-from-strided-arrays strided-arrays ir-backend)))
    (mapc #'execute root-buffers)
    (mapcar #'immediate-from-buffer root-buffers)))

(defmethod immediate-from-buffer ((ir-backend-buffer ir-backend-buffer))
  (make-array-immediate (storage ir-backend-buffer)))

(defmethod execute :before ((buffer petalisp-ir:buffer))
  (mapc #'execute (inputs buffer)))

(defmethod execute :before ((kernel petalisp-ir:kernel))
  (mapc (compose #'execute #'car)
        (petalisp-ir:loads kernel)))

(defmethod execute :around ((ir-backend-buffer ir-backend-buffer))
  (unless (executedp ir-backend-buffer)
    (call-next-method)
    (setf (executedp ir-backend-buffer) t)))

(defmethod execute ((ir-backend-buffer ir-backend-buffer))
  (mapc #'execute (inputs ir-backend-buffer)))

(defmethod execute ((kernel petalisp-ir:kernel))
  (funcall (compile-kernel kernel)))
