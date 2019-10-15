;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defun number-of-processors ()
  (handler-case
      (parse-integer
       (with-output-to-string (*standard-output*)
         (uiop:run-program "nproc" :output t)))
    (uiop:subprocess-error () 1)))

;;; This is the default Petalisp backend.  It generates portable, highly
;;; optimized Lisp code and compiles it using CL:COMPILE.

(defclass native-backend (asynchronous-backend)
  ((%memory-pool :initarg :memory-pool :reader memory-pool)
   (%worker-pool :initarg :worker-pool :reader worker-pool)
   (%compile-cache :initarg :compile-cache :reader compile-cache
                   :initform (make-hash-table :test #'eq))))

(defun make-native-backend (&key (threads (number-of-processors)))
  (check-type threads alexandria:positive-integer)
  (make-instance 'native-backend
    :memory-pool (make-memory-pool)
    :worker-pool (make-worker-pool threads)))

(defmethod delete-backend ((native-backend native-backend))
  (delete-worker-pool (worker-pool native-backend)))
