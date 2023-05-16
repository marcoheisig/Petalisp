;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defgeneric backend-worker-pool (backend))

(defgeneric backend-compile-cache (backend))

(defgeneric backend-compile-blueprint (backend blueprint))

(defclass backend (petalisp.core:backend)
  ((%worker-pool
    :initform (alexandria:required-argument :worker-pool)
    :initarg :worker-pool
    :type worker-pool
    :reader backend-worker-pool)
   (%compile-cache
    :initarg :compile-cache
    :initform (make-hash-table :test #'eq)
    :reader backend-compile-cache)))

(defun make-native-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads (integer 1))
  (make-instance 'backend
    :worker-pool (make-worker-pool threads)))

(defmethod backend-compile-blueprint
    ((backend backend)
     (blueprint t))
  (alexandria:ensure-gethash
   blueprint
   (backend-compile-cache backend)
   (compile nil (translate-blueprint backend blueprint))))

(defmethod delete-backend
    ((backend backend))
  (worker-pool-join
   (backend-worker-pool backend)))
