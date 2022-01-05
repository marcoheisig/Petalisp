;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;; This is the default Petalisp backend.  It generates portable, highly
;;; optimized Lisp code and compiles it using CL:COMPILE.

(defclass native-backend (backend)
  ((%scheduler-queue :initform (lparallel.queue:make-queue) :reader scheduler-queue)
   (%scheduler-thread :accessor scheduler-thread)
   (%memory-pool :initarg :memory-pool :reader memory-pool)
   (%worker-pool :initarg :worker-pool :reader worker-pool)
   (%compile-cache :initarg :compile-cache :reader compile-cache
                   :initform (make-hash-table :test #'eq))))

(defun make-native-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads alexandria:positive-integer)
  (make-instance 'native-backend
    :memory-pool (make-memory-pool)
    :worker-pool (make-worker-pool threads)))

(defmethod delete-backend ((native-backend native-backend))
  (with-accessors ((queue scheduler-queue)
                   (thread scheduler-thread)) native-backend
    (lparallel.queue:push-queue :quit queue)
    (bt:join-thread thread))
  (delete-worker-pool (worker-pool native-backend)))

(defmethod initialize-instance :after
    ((native-backend native-backend) &key &allow-other-keys)
  (let ((queue (scheduler-queue native-backend)))
    (setf (scheduler-thread native-backend)
          (bt:make-thread
           (lambda ()
             (loop for item = (lparallel.queue:pop-queue queue) do
               (if (functionp item)
                   (funcall item)
                   (loop-finish))))
           :name (format nil "~A scheduler thread"
                         (class-name (class-of native-backend)))))))

(defmethod backend-schedule
    ((backend native-backend)
     (lazy-arrays list)
     (finalizer function))
  (let ((promise (lparallel.promise:promise)))
    (lparallel.queue:push-queue
     (lambda ()
       (lparallel.promise:fulfill promise
         (funcall finalizer (backend-compute backend lazy-arrays))))
     (scheduler-queue backend))
    promise))

(defmethod backend-wait
    ((backend native-backend)
     (requests list))
  (mapc #'lparallel.promise:force requests))
