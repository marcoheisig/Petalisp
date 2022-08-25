;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defgeneric starpu-backend-kernel-codelet (backend kernel))

(defclass starpu-backend (backend)
  (;; A hash table, mapping from kernel blueprints to StarPU codelets.
   (%blueprint-codelets
    :initform (make-hash-table :test #'eq)
    :type hash-table
    :reader starpu-backend-blueprint-codelets)))

(defun make-starpu-backend ()
  (make-instance 'starpu-backend))

(defmethod starpu-backend-kernel-codelet ((starpu-backend starpu-backend) (kernel kernel))
  (let ((blueprint (kernel-blueprint kernel)))
    (alexandria:ensure-gethash
     blueprint
     (starpu-backend-blueprint-codelets starpu-backend)
     (blueprint-codelet blueprint))))
