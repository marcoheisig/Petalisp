;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defgeneric devicep (device))

(defgeneric device-name (device))

(defgeneric device-number-of-workers (device))

(defgeneric device-memory-size (device))

(defgeneric compile-blueprint (blueprint device))

(defgeneric allocate-buffer (buffer device))

(defgeneric deallocate-buffer (buffer))

(defgeneric move-buffer (buffer from-device to-device))

(defclass device ()
  ((%name
    :initarg :name
    :reader device-name
    :type string
    :initform (alexandria:required-argument :name))
   ;; The number of workers, e.g., the number of cores of a CPU, or the
   ;; number of compute units of a GPU.
   (%workers
    :initarg :number-of-workers
    :reader device-number-of-workers
    :type (integer 1 *)
    :initform (alexandria:required-argument :number-of-workers))
   ;; The size of the available memory, in bytes.
   (%memory-size
    :initarg :memory-size
    :reader device-memory-size
    :type (integer 1 *)
    :initform (alexandria:required-argument :memory-size))))

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type t)
    (write-string (device-name device) stream)))

(defmethod devicep ((device device))
  (declare (ignore device))
  t)

(defmethod devicep (object)
  (declare (ignore object))
  nil)
