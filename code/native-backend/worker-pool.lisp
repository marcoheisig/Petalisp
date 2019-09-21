;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
                 (let ((*worker-id* index))
                   (catch 'join-thread
                     (loop
                       (funcall
                        (lparallel.queue:pop-queue queue))))))
               :name (format nil "Petalisp worker thread #~D" index)))))
    (%make-worker-pool threads queues)))

(defun worker-pool-size (worker-pool)
  (length
   (worker-pool-threads worker-pool)))

(defun execute-in-worker-pool (function worker-pool)
  (loop for thread across (worker-pool-threads worker-pool)
        for queue across (worker-pool-queues worker-pool) do
          (lparallel.queue:push-queue
           (lambda ()
             (funcall function *worker-id*))
           queue)))

(defun delete-worker-pool (worker-pool)
  (execute-in-worker-pool
   (lambda (worker-id)
     (declare (ignore worker-id))
     (throw 'join-thread (values)))
   worker-pool)
  (map nil #'bt:join-thread (worker-pool-threads worker-pool)))
