;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defvar *worker-id*)

(defstruct (worker-pool
            (:predicate worker-pool-p)
            (:constructor %make-worker-pool (threads queues))
            (:conc-name worker-pool-))
  (threads nil :type simple-vector)
  (queues nil :type simple-vector))

(defun make-worker-pool (n-threads)
  (let ((threads (make-array n-threads))
        (queues (make-array n-threads)))
    (dotimes (index n-threads)
      (let ((queue (lparallel.queue:make-queue)))
        (setf (svref queues index)
              queue)
        (setf (svref threads index)
              (bt:make-thread
               (lambda ()
                 (catch 'join-thread
                   (loop
                     (funcall (lparallel.queue:pop-queue queue) *worker-id*))))
               :name (format nil "Petalisp worker pool thread #~D" index)
               :initial-bindings `((*worker-id* . ,index))))))
    (%make-worker-pool threads queues)))

(defun worker-pool-size (worker-pool)
  (length
   (worker-pool-threads worker-pool)))

(defun worker-pool-enqueue (function worker-pool)
  (loop for queue across (worker-pool-queues worker-pool) do
    (lparallel.queue:push-queue function queue)))

(let ((semaphore (bt:make-semaphore :name "Petalisp worker pool wait semaphore")))
  (defun worker-pool-wait (worker-pool)
    (worker-pool-enqueue
     (lambda (worker-id)
       (declare (ignore worker-id))
       (bt:signal-semaphore semaphore))
     worker-pool)
    (loop repeat (worker-pool-size worker-pool) do
      (bt:wait-on-semaphore semaphore))))

(defun delete-worker-pool (worker-pool)
  (worker-pool-enqueue
   (lambda (worker-id)
     (declare (ignore worker-id))
     (throw 'join-thread nil))
   worker-pool)
  (loop for thread across (worker-pool-threads worker-pool)
        do (bt:join-thread thread)))
