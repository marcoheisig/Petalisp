;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.xmas-backend)

(defclass xmas-backend (backend)
  ((%memory-pool
    :initform (make-memory-pool)
    :type memory-pool
    :reader xmas-backend-memory-pool)
   (%lparallel-kernel
    :initform (alexandria:required-argument :lparallel-kernel)
    :initarg :lparallel-kernel
    :type lparallel:kernel
    :reader xmas-backend-lparallel-kernel)
   (%compile-cache
    :initarg :compile-cache
    :initform (make-hash-table :test #'eq)
    :reader xmas-backend-compile-cache)))

(defun make-xmas-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads (integer 1))
  (make-instance 'native-backend
    :memory-pool (make-memory-pool)
    :lparallel-kernel
    (lparallel:make-kernel threads :name "Petalisp Xmas Backend")))

(defmethod delete-backend ((xmas-backend xmas-backend))
  (let ((lparallel:*kernel* (xmas-backend-lparallel-kernel xmas-backend)))
    (lparallel:end-kernel :wait t))
  (call-next-method))

(defmethod backend-compute
    ((xmas-backend xmas-backend)
     (lazy-arrays list))
  (break "TODO"))
