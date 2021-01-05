;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.multicore-backend)

(defconstant +max-workers+ (expt 2 16))

(deftype worker-pool-size ()
  `(integer 1 ,+max-workers+))

(deftype worker-pool-workers ()
  `simple-vector)

(deftype worker-id ()
  `(integer 0 (,+max-workers+)))

(defstruct (worker-pool
            (:copier nil)
            (:predicate worke-pool-p)
            (:constructor %make-worker-pool
                (size &aux (workers (make-array size :initial-element nil)))))
  ;; The number of workers in the worker pool.
  (size nil
   :type worker-pool-size
   :read-only t)
  ;; The vector of workers.
  (workers nil
   :type worker-pool-workers
   :read-only t)
  ;; The state of the worker pool.
  (state :active
   :type (member :active :deleted))
  ;; A FIFO queue of thunks.  Workers grab these thunks whenever their
  ;; local queues and those of their neighbors are exhausted.
  (queue (petalisp.utilities:make-queue)
   :type petalisp.utilities:queue
   :read-only t))

(defstruct (worker
            (:copier nil)
            (:predicate workerp)
            (:constructor make-worker (id worker-pool)))
  (id nil
   :type worker-id
   :read-only t)
  (worker-pool nil
   :type worker-pool
   :read-only t)
  (wsdeque (petalisp.utilities:make-wsdeque)
   :type petalisp.utilities:wsdeque
   :read-only t)
  (thread nil
   :type (or bordeaux-threads:thread null)))

(declaim (worker *worker*))
(defvar *worker*) ; Bound within each worker thread.

(defun make-worker-pool (size)
  (check-type size worker-pool-size)
  (let* ((worker-pool (%make-worker-pool size))
         (workers (worker-pool-workers worker-pool)))
    (loop for id from 0 below size do
      (setf (svref workers id)
            (make-worker id worker-pool)))
    ;; Only start workers once all of them have been created.  Otherwise,
    ;; they'll try to steal from neighbors that don't exist.
    (map nil #'start-worker workers)
    worker-pool))

(defun start-worker (worker)
  (setf (worker-thread worker)
        (bordeaux-threads:make-thread
         #'worker
         :name (format nil "Worker ~D" (worker-id worker))
         :initial-bindings
         (list* `(*worker* . ,worker) bordeaux-threads:*default-special-bindings*))))

;;; The entry function for each new worker thread.
(defun worker ()
  (handler-bind
      ((serious-condition
         (lambda (condition)
           (when (eq (worker-pool-state (worker-worker-pool *worker*)) :deleted)
             (warn "Ignoring ~S because it occurred in a deleted worker pool."
                   condition)
             (return-from worker (values))))))
    ;; This loop only advances when the ABORT restart is activated.
    (tagbody retry
       (restart-case (worker-loop)
         (abort ()
           :report "Delete the worker pool."
           (setf (worker-pool-state (worker-worker-pool *worker*))
                 :deleted)
           (go retry))))))

(defun worker-loop ()
  (with-accessors ((id worker-id)
                   (worker-pool worker-worker-pool)
                   (wsdeque worker-wsdeque)) *worker*
    (with-accessors ((workers worker-pool-workers)
                     (state worker-pool-state)
                     (queue worker-pool-queue)) worker-pool
      (loop with consecutive-yields fixnum = 0 do
        (block block
          (labels ((probe-thunk (thunk)
                     (etypecase thunk
                       (function
                        (funcall thunk)
                        (setf consecutive-yields 0)
                        (return-from block))
                       (null (values))))
                   (steal-from (neighbor-id)
                     (let* ((neighbor (svref workers neighbor-id))
                            (neighbor-wsdeque (worker-wsdeque neighbor)))
                       (probe-thunk
                        (petalisp.utilities:wsdeque-steal neighbor-wsdeque)))))
            ;; Attempt to invoke the next thunk from the local wsdeque.
            (probe-thunk
             (petalisp.utilities:wsdeque-pop wsdeque))
            ;; If the local wsdeque is empty, attempt to steal work from
            ;; another worker in the pool.
            (loop for neighbor-id from (1+ id) below (length workers) do
              (steal-from neighbor-id))
            (loop for neighbor-id from 0 below id do
              (steal-from neighbor-id))
            ;; If no work can be stolen from the neighbors, have a look
            ;; at the worker pool's queue.
            (probe-thunk
             (petalisp.utilities:queue-dequeue queue))
            ;; If there is no work left, we yield.
            (cond
              ((eq state :deleted)
               (return-from worker-loop (values)))
              ((<= consecutive-yields 64)
               (bordeaux-threads:thread-yield))
              ((<= consecutive-yields 10064)
               (sleep
                (* 0.02 (/ (- consecutive-yields 64) 10000.0))))
              (t
               (sleep 0.02)))
            (incf consecutive-yields)))))))

(defun enqueue-thunk (thunk)
  (check-type thunk function)
  (petalisp.utilities:wsdeque-push
   (worker-wsdeque *worker*)
   thunk))

(defun worker-pool-enqueue-thunk (worker-pool thunk)
  (check-type thunk function)
  (with-accessors ((queue worker-pool-queue)) worker-pool
    (petalisp.utilities:queue-enqueue queue thunk)))

(defun delete-worker-pool (worker-pool)
  (setf (worker-pool-state worker-pool)
        :deleted)
  (loop for worker across (worker-pool-workers worker-pool) do
    (bordeaux-threads:join-thread (worker-thread worker))))

(let ((lock (bordeaux-threads:make-lock)))
  (defun worker-message (format-string &rest arguments)
    (bordeaux-threads:with-lock-held (lock)
      (format *trace-output* "~&Worker ~D: " (worker-id *worker*))
      (apply #'format *trace-output* format-string arguments)
      (finish-output))))

(defmacro asynchronously (&body body)
  `(enqueue-thunk
    (lambda () ,@body)))
