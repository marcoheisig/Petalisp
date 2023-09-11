;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;; A type such that struct slots with that type can be modified via
;;; atomics:atomic-incf.
(deftype atomic-counter ()
  #+(or ecl mezzano) 'fixnum
  #+sbcl 'sb-ext:word
  #+ccl 't
  #-(or ecl mezzano sbcl ccl) 'integer)

(deftype worker-id ()
  `(and unsigned-byte fixnum))

(defstruct (worker-pool
            (:copier nil)
            (:predicate worker-pool-p)
            (:constructor %make-worker-pool
                (size &aux
                        (workers (make-array size :initial-element nil))
                        (barrier-countdown size))))
  ;; The vector of workers.
  (workers nil
   :type simple-vector
   :read-only t)
  ;; A boolean that is set to NIL once the worker pool's threads are deleted.
  (active t :type boolean)
  ;; The sense of this worker pool's barrier. It switches atomically
  ;; between :A and :B whenever its worker threads run into a barrier.
  (barrier-sense :A
   :type (member :A :B))
  (barrier-countdown 0
   :type atomic-counter))

(defun worker-pool-size (worker-pool)
  (length (worker-pool-workers worker-pool)))

(defun worker-pool-worker (worker-pool worker-id)
  (declare (worker-pool worker-pool))
  (declare (worker-id worker-id))
  (svref (worker-pool-workers worker-pool) worker-id))

(defstruct (worker
            (:copier nil)
            (:predicate workerp)
            (:constructor %make-worker (id worker-pool)))
  ;; The worker pool that contains this worker.
  (worker-pool nil
   :type worker-pool
   :read-only t)
  ;; The worker's number in its containing worker pool.
  (id nil
   :type worker-id
   :read-only t)
  ;; The thread that has its *worker* special variable bound to this worker
  ;; object.
  (thread nil
   :type (or bordeaux-threads-2:thread null))
  ;; A queue of thunks that are scheduled for execution on this worker.
  (queue (lparallel.queue:make-queue))
  ;; The sense of the barrier as last observed by this worker.
  (barrier-sense :B
   :type (member :A :B))
  ;; A list of all the serious conditions that were caught (and ignored) by
  ;; the worker.
  (serious-conditions '()
   :type list))

(defmethod print-object ((worker worker) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of worker))
          :id (worker-id worker)
          :barrier-sense (worker-barrier-sense worker)
          :serious-conditions (worker-serious-conditions worker)))

(declaim (worker *worker*))
(defvar *worker*) ; Bound within each worker thread.

;;; Block the worker until all other workers have also called the BARRIER
;;; function.
(defun barrier ()
  (with-accessors ((worker-pool worker-worker-pool)
                   (local-sense worker-barrier-sense)) *worker*
    (with-accessors ((size worker-pool-size)
                     (countdown worker-pool-barrier-countdown)
                     (global-sense worker-pool-barrier-sense)) worker-pool
      (if (zerop (atomics:atomic-decf countdown))
          (setf countdown size global-sense local-sense)
          (loop until (eq local-sense global-sense)))
      (ecase local-sense
        (:A (setf local-sense :B))
        (:B (setf local-sense :A))))))

(defun make-worker-pool (size)
  (let* ((worker-pool (%make-worker-pool size)))
    (loop for id from 0 below size do
      (setf (svref (worker-pool-workers worker-pool) id)
            (%make-worker id worker-pool)))
    (map nil #'start-worker (worker-pool-workers worker-pool))
    worker-pool))

(defun start-worker (worker)
  (setf (worker-thread worker)
        (bordeaux-threads-2:make-thread
         #'worker-loop
         :name (format nil "Worker Thread ~D" (worker-id worker))
         :initial-bindings
         (list* `(*worker* . ,worker) bordeaux-threads-2:*default-special-bindings*))))

;;; The entry function for each new worker thread.
(defun worker-loop ()
  ;; Pin the worker thread.
  (pin-current-thread (worker-id *worker*))
  ;; Wait until all other threads have been started, too.
  (barrier)
  ;; Process thunks until the worker is told to stop.
  (let ((queue (worker-queue *worker*)))
    (catch 'join
      (loop
        (handler-case (loop (funcall (lparallel.queue:pop-queue queue)))
          #+(or)
          (serious-condition (c)
            (push c (worker-serious-conditions *worker*))))))))

(defun worker-enqueue (worker thunk)
  (declare (worker worker) (function thunk))
  (lparallel.queue:push-queue thunk (worker-queue worker)))

(defun worker-pool-join (worker-pool)
  (declare (worker-pool worker-pool))
  (loop
    (cond ((not (worker-pool-active worker-pool))
           (return-from worker-pool-join nil))
          ((worker-pool-active worker-pool)
           (when (atomics:cas (worker-pool-active worker-pool) t nil)
             (loop for id from 0 below (worker-pool-size worker-pool) do
               (worker-enqueue
                (worker-pool-worker worker-pool id)
                (lambda ()
                  (throw 'join nil))))
             (loop for id from 0 below (worker-pool-size worker-pool) do
               (bordeaux-threads-2:join-thread
                (shiftf (worker-thread (worker-pool-worker worker-pool id))
                        nil)))
             (return-from worker-pool-join t))))))

(let ((lock (bordeaux-threads-2:make-lock)))
  (defun message (format-string &rest arguments)
    (bordeaux-threads-2:with-lock-held (lock)
      (format *trace-output* "~&Worker ~D: " (worker-id *worker*))
      (apply #'format *trace-output* format-string arguments)
      (finish-output))))
