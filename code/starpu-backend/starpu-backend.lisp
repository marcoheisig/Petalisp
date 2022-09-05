;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defparameter *kernel-scaling-threshold* 3)
(defparameter *kernel-offset-threshold* 2)

(defgeneric starpu-backend-kernel-codelet (backend kernel))

(defclass starpu-backend (backend)
  (;; A hash table, mapping from kernel blueprints to StarPU codelets.
   (%blueprint-codelets
    :initform (make-hash-table :test #'eq)
    :type hash-table
    :reader starpu-backend-blueprint-codelets)))

(defun make-starpu-backend ()
  (make-instance 'starpu-backend))

(defmethod initialize-instance :before
    ((starpu-backend starpu-backend) &key &allow-other-keys)
  (unless (starpu:initializedp)
    (starpu:initialize)
    (starpu:pause)))
