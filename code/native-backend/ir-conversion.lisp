;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass buffer (petalisp.ir:buffer)
  ())

(defclass non-immediate-buffer (buffer)
  ((%storage :initarg :storage :accessor storage)))

(defclass immediate-buffer (buffer)
  ())

(defclass array-buffer (immediate-buffer)
  ((%storage :initarg :storage :reader storage)))

(defclass kernel (petalisp.ir:kernel)
  ((%executedp :initarg :executedp :accessor executedp
               :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod petalisp.ir:make-buffer
    ((array-immediate array-immediate)
     (native-backend native-backend))
  (make-instance 'array-buffer
    :shape (shape array-immediate)
    :type-code (type-code array-immediate)
    :storage (storage array-immediate)))

(defmethod petalisp.ir:make-buffer
    ((lazy-array lazy-array)
     (native-backend native-backend))
  (make-instance 'non-immediate-buffer
    :shape (shape lazy-array)
    :type-code (type-code lazy-array)))

(defmethod petalisp.ir:make-kernel
    ((backend native-backend) &rest args)
  (apply #'make-instance 'kernel args))
