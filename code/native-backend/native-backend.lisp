;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;; This is the default Petalisp backend.  It generates portable, highly
;;; optimized Lisp code and compiles it using CL:COMPILE.

(defclass native-backend (asynchronous-backend)
  ((%memory-pool :initarg :memory-pool :reader memory-pool)
   (%worker-pool :initarg :worker-pool :reader worker-pool)
   (%compile-cache :initarg :compile-cache :reader compile-cache
                   :initform (make-hash-table :test #'eq))))

(defun make-native-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads alexandria:positive-integer)
  (make-instance 'native-backend
    :machine (host-machine)
    :memory-pool (make-memory-pool)
    :worker-pool (make-worker-pool threads)))

(defmethod delete-backend ((native-backend native-backend))
  (delete-worker-pool (worker-pool native-backend)))
