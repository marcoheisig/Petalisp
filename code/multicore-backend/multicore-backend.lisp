;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.multicore-backend)

(defclass multicore-backend (backend)
  ((%worker-pool
    :initarg :worker-pool
    :reader multicore-backend-worker-pool
    :type worker-pool)
   (%kernel-info-lock
    :reader multicore-backend-kernel-info-lock
    :initform (bordeaux-threads:make-lock "Kernel Info Lock")
    :type (bordeaux-threads:lock))
   (%kernel-info-cache
    :reader multicore-backend-kernel-info-cache
    :initform (make-hash-table :test #'eq)
    :type hash-table)
   (%ucons-root-table
    :reader multicore-backend-ucons-root-table
    :initform (ucons:make-root-table)
    :type ucons:root-table)))

(defun make-multicore-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads unsigned-byte)
  (make-instance 'multicore-backend
    :worker-pool (make-worker-pool threads)))

(defmethod delete-backend ((backend multicore-backend))
  (delete-worker-pool
   (multicore-backend-worker-pool backend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal Data Structures

(deftype atomic-counter ()
  #+sbcl 'sb-ext:word
  #-sbcl 'fixnum)

(defstruct request
  ;; The backend to which this request was made.
  (backend nil
   :type backend
   :read-only t)
  ;; The roots of the data flow graph whose execution has been requested.
  (lazy-arrays nil
   :type list
   :read-only t)
  ;; The third argument that was passed to BACKEND-SCHEDULE.
  (finalizer nil
   :type function
   :read-only t)
  ;; The lock and cvar are used to communicate whether the request has been
  ;; finished.
  (lock (bordeaux-threads:make-lock "Multicore Backend Request Lock")
   :type bordeaux-threads:lock
   :read-only t)
  (cvar (bordeaux-threads:make-condition-variable)
   :type t
   :read-only t)
  ;; The number of IR roots that still need to be computed.  Later,
  ;; each operation that computes the value of an IR root will
  ;; decrement this counter.  The operation that decrements this
  ;; counter to zero will also notify anyone waiting on the request.
  (pending-roots nil
   :type atomic-counter)
  ;; The roots of the IR graph.
  (ir-roots '()))

(defstruct buffer-data
  ;; The request that this buffer is being part of.
  (request nil
   :type request
   :read-only t)
  ;; The number of kernels that have yet to write into this buffer.
  (pending-inputs nil
   :type atomic-counter)
  ;; The number of kernels that have yet to read from this buffer.
  (pending-outputs nil
   :type atomic-counter)
  ;; A buffer that has the same shape and ntype as this buffer, and that
  ;; appears later (i.e., closer to one of the IR roots) than this buffer.
  ;; A descendant of NIL means there is no such buffer.
  (descendant nil
   :type (or null petalisp.ir:buffer)))

(defstruct kernel-data
  ;; The request that this kernel is being part of.
  (request nil
   :type request
   :read-only t)
  ;; The number of input buffers with a nonzero number of pending writes.
  (pending-buffers nil
   :type atomic-counter)
  ;; A list of thunks that have to be executed to evaluate the kernel.
  (tasks nil
   :type list)
  ;; A kernel goes through multiple states while being executed.  It starts
  ;; in the :INITIAL state.  The first thread that starts preprocessing the
  ;; kernel changes its state to :PREPROCESSING, and, once that is
  ;; complete, to :PREPROCESSED.  Once a kernel has zero pending buffers,
  ;; it is ready for execution.  A thread that wants to execute the kernel
  ;; will first ensure it is in the :PREPROCESSED state (either by busy
  ;; waiting, or by preprocessing it itself).  Then, it will change the
  ;; state to :EXECUTING and enqueue all its tasks.  Each task finishes by
  ;; decrementing the kernel's pending elements by the size of its
  ;; iteration space.  The task that sets the number of pending elements to
  ;; zero sets the kernel's state to :EXECUTED and decrements the pending
  ;; reads.  When it sets a buffer's number of pending reads to zero, it
  ;; decrements the number of pending buffers of each kernel reading from
  ;; that buffer.  When it sets a kernel's number of pending buffers to
  ;; zero, the kernel is enqueued for execution.
  (state :initial
   :type (member :initial :preprocessing :preprocessed :executing :executed)))

(defstruct kernel-info
  ;; The compiled kernel.
  (compiled-kernel nil
   :type function
   :read-only t)
  ;; The ideal task size for that kernel.
  (task-size nil
   :type unsigned-byte
   :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execution

(defun dummy-finalizer (immediates)
  (declare (ignore immediates)))

(defmethod backend-compute
    ((backend multicore-backend)
     (lazy-arrays list))
  (let ((request (backend-schedule backend lazy-arrays #'dummy-finalizer)))
    (backend-wait backend (list request))
    (mapcar #'petalisp.ir:buffer-storage (request-ir-roots request))))

(defmethod backend-schedule
    ((backend multicore-backend)
     (lazy-arrays list)
     (finalizer function))
  (let ((request (make-request
                  :backend backend
                  :lazy-arrays lazy-arrays
                  :finalizer finalizer
                  :pending-roots (length lazy-arrays))))
    (worker-pool-enqueue-thunk
     (multicore-backend-worker-pool backend)
     (lambda ()
       (handle-request request)))
    request))

(defmethod backend-wait
    ((backend multicore-backend)
     (requests list))
  (loop for request in requests do
    (with-accessors ((lock request-lock)
                     (cvar request-cvar)
                     (pending-roots request-pending-roots)) request
      (unless (zerop pending-roots)
        (bordeaux-threads:with-lock-held (lock)
          (bordeaux-threads:condition-wait cvar lock)
          (assert (zerop pending-roots)))))))

(defvar *available-kernels*)
(defvar *request*)
(defvar *allocation-table*)

(defun handle-request (request)
  (with-accessors ((lazy-arrays request-lazy-arrays)
                   (ir-roots request-ir-roots)) request
    (let ((*available-kernels* '())
          (*request* request)
          (*allocation-table* (make-allocation-table)))
      (setf ir-roots (petalisp.ir:ir-from-lazy-arrays lazy-arrays))
      ;; Initialize the DATA slot of each buffer and each kernel.  Also,
      ;; push all kernels with zero unevaluated dependencies to the special
      ;; variable *AVAILABLE-KERNELS*.
      (mapc #'initialize-buffer ir-roots)
      (setf *allocation-table* nil)
      ;; Start processing kernels.
      (mapcar
       (lambda (kernel)
         (asynchronously
           (preprocess-kernel kernel)))
       *available-kernels*)
      ;; Start executing kernels.
      (mapcar
       (lambda (kernel)
         (asynchronously
           (execute-kernel kernel)))
       *available-kernels*))))

(defun initialize-buffer (buffer)
  (with-accessors ((shape petalisp.ir:buffer-shape)
                   (ntype petalisp.ir:buffer-ntype)
                   (data petalisp.ir:buffer-data)) buffer
    (when (null data)
      (let ((descendant
              ;; We only track descendants of 'large' buffers.
              (if (< (shape-size shape) 200)
                  nil
                  (shiftf (allocation-table-value *allocation-table* shape ntype)
                          buffer))))
        (setf (petalisp.ir:buffer-data buffer)
              (make-buffer-data
               :request *request*
               :descendant descendant
               :pending-inputs (petalisp.ir:buffer-number-of-inputs buffer)
               :pending-outputs (petalisp.ir:buffer-number-of-outputs buffer))))
      (petalisp.ir:map-buffer-inputs #'initialize-kernel buffer))))

(defun initialize-kernel (kernel)
  (with-accessors ((kernel-data petalisp.ir:kernel-data)
                   (number-of-inputs petalisp.ir:kernel-number-of-inputs)
                   (iteration-space petalisp.ir:kernel-iteration-space)) kernel
    (when (null kernel-data)
      (let ((pending-buffers 0))
        (petalisp.ir:map-kernel-inputs
         (lambda (buffer)
           (unless (petalisp.ir:leaf-buffer-p buffer)
             (incf pending-buffers)))
         kernel)
        (setf kernel-data (make-kernel-data
                           :request *request*
                           :pending-buffers pending-buffers))
        (when (zerop pending-buffers)
          (push kernel *available-kernels*))
        (petalisp.ir:map-kernel-inputs #'initialize-buffer kernel)))))

;;; Ensure that a kernel is partitioned into tasks.
(defun preprocess-kernel (kernel)
  (with-accessors ((data petalisp.ir:kernel-data)) kernel
    (with-accessors ((tasks kernel-data-tasks)
                     (state kernel-data-state)) data
      (when (atomics:cas state :initial :preprocessing)
        (assert (atomics:cas tasks '() (compute-kernel-tasks kernel)))
        (when (atomics:cas state :preprocessing :preprocessed)
          (petalisp.ir:map-kernel-outputs
           (lambda (buffer)
             (petalisp.ir:map-buffer-outputs #'preprocess-kernel buffer))
           kernel))))))

;;; Ensure that all tasks of the kernel are queued for execution.
(defun execute-kernel (kernel)
  (with-accessors ((data petalisp.ir:kernel-data)) kernel
    (with-accessors ((pending-buffers kernel-data-pending-buffers)
                     (tasks kernel-data-tasks)
                     (state kernel-data-state)) data
      (loop do
        (ecase state
          (:initial (preprocess-kernel kernel))
          (:preprocessing)
          (:preprocessed
           (when (atomics:cas state :preprocessed :executing)
             (petalisp.ir:map-kernel-outputs #'allocate-buffer kernel)
             (mapc #'enqueue-thunk tasks))
           (loop-finish))
          ((:executing :executed) (loop-finish)))))))

(defconstant +interpreter-threshold+ 20)

(defun compute-kernel-tasks (kernel)
  (with-accessors ((iteration-space petalisp.ir:kernel-iteration-space)) kernel
    (if (< (shape-size iteration-space) +interpreter-threshold+)
        ;; When the iteration space is small, we don't generate only a
        ;; single task that uses the kernel interpreter.
        (list
         (lambda ()
           (petalisp.ir:interpret-kernel kernel iteration-space)
           (finalize-kernel kernel)))
        ;; When the iteration space is not small, we compile it and
        ;; partition its iteration space into one or more tasks.
        (let* ((kernel-info (kernel-info kernel))
               (task-size (kernel-info-task-size kernel-info))
               (compiled-kernel (kernel-info-compiled-kernel kernel-info))
               (cell (cons '.task-countdown. 0))
               (tasks
                 (mapcar
                  (lambda (iteration-space)
                    (lambda ()
                      (funcall compiled-kernel kernel iteration-space)
                      (when (zerop (atomics:atomic-decf (cdr cell)))
                        (finalize-kernel kernel))))
                  (partition-iteration-space iteration-space task-size))))
          (assert (atomics:atomic-incf (cdr cell) (length tasks)))
          tasks))))

(defun partition-iteration-space (iteration-space task-size)
  (declare (type shape iteration-space)
           (type unsigned-byte task-size))
  (if (<= (shape-size iteration-space) task-size)
      (list iteration-space)
      (let* ((ranges (shape-ranges iteration-space))
             (max-axis 0)
             (max-range (first ranges)))
        (loop for range in (rest ranges) and axis from 1
              when (> (range-size range)
                      (range-size max-range))
                do (setf max-axis axis max-range range))
        (let ((prefix (subseq ranges 0 max-axis))
              (suffix (subseq ranges (1+ max-axis))))
          (multiple-value-bind (lo hi) (split-range max-range)
            (append
             (partition-iteration-space
              (make-shape (append prefix (list lo) suffix))
              task-size)
             (partition-iteration-space
              (make-shape (append prefix (list hi) suffix))
              task-size)))))))

(defun finalize-kernel (kernel)
  ;; Ensure each kernel is only finalized once.
  (assert (atomics:cas (kernel-data-state (petalisp.ir:kernel-data kernel))
                       :executing
                       :executed))
  ;; Decrement the number of pending outputs of each output buffer.
  (petalisp.ir:map-kernel-inputs
   (lambda (buffer)
     (with-accessors ((buffer-data petalisp.ir:buffer-data)) buffer
       (with-accessors ((pending-outputs buffer-data-pending-outputs)
                        (request buffer-data-request)) buffer-data
         (when (zerop (atomics:atomic-decf pending-outputs))
           (unless (petalisp.ir:leaf-buffer-p buffer)
             (deallocate-buffer buffer))))))
   kernel)
  ;; Decrement the number of pending inputs of each output buffer.
  (petalisp.ir:map-kernel-outputs
   (lambda (buffer)
     (with-accessors ((buffer-data petalisp.ir:buffer-data)) buffer
       (with-accessors ((pending-inputs buffer-data-pending-inputs)
                        (request buffer-data-request)) buffer-data
         ;; If one of these counters reaches zero, check whether now
         ;; kernels have become available, or whether a root buffer has
         ;; been completed.
         (when (zerop (atomics:atomic-decf pending-inputs))
           (if (petalisp.ir:root-buffer-p buffer)
               ;; If we are dealing with a root buffer, signal that it has
               ;; been completed.
               (when (zerop (atomics:atomic-decf (request-pending-roots request)))
                 (bordeaux-threads:with-lock-held ((request-lock request))
                   (funcall (request-finalizer request)
                            (mapcar #'petalisp.ir:buffer-storage
                                    (request-ir-roots request)))
                   (bordeaux-threads:condition-notify (request-cvar request))))
               ;; If we are dealing with an interior buffer, we decrement
               ;; the number of pending buffers of each kernel that reads
               ;; from it.
               (petalisp.ir:map-buffer-outputs
                (lambda (kernel)
                  (with-accessors ((kernel-data petalisp.ir:kernel-data)) kernel
                    (with-accessors ((pending-buffers kernel-data-pending-buffers)) kernel-data
                      (when (zerop (atomics:atomic-decf pending-buffers))
                        ;; When a kernel has no more pending buffers, it can be
                        ;; executed.
                        (asynchronously (execute-kernel kernel))))))
                buffer))))))
   kernel))

(defun allocate-buffer (buffer)
  (with-accessors ((shape petalisp.ir:buffer-shape)
                   (ntype petalisp.ir:buffer-ntype)
                   (storage petalisp.ir:buffer-storage)) buffer
    (when (null storage)
      (let* ((element-type (petalisp.type-inference:type-specifier ntype))
             (array (make-array (shape-dimensions shape)
                                :element-type element-type)))
        (atomics:cas storage nil array)))))

(defun deallocate-buffer (buffer)
  (with-accessors ((storage petalisp.ir:buffer-storage)) buffer
    (labels ((donate-memory (memory descendant)
               (unless (null descendant)
                 (unless (atomics:cas (petalisp.ir:buffer-storage descendant) nil memory)
                   (donate-memory
                    memory
                    (buffer-data-descendant
                     (petalisp.ir:buffer-data descendant)))))))
      (donate-memory
       (shiftf storage nil)
       (buffer-data-descendant (petalisp.ir:buffer-data buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Info Lookup

(defun kernel-info (kernel)
  (let* ((request (kernel-data-request (petalisp.ir:kernel-data kernel)))
         (backend (request-backend request))
         (cache (multicore-backend-kernel-info-cache backend))
         (blueprint
           (let ((ucons:*root-table* (multicore-backend-ucons-root-table backend)))
             (petalisp.ir:kernel-blueprint kernel))))
    ;; Check whether we already have a cached kernel info for this
    ;; blueprint.  If so, return it.
    (bordeaux-threads:with-lock-held ((multicore-backend-kernel-info-lock backend))
      (multiple-value-bind (kernel-info presentp)
          (gethash blueprint cache)
        (when presentp (return-from kernel-info kernel-info))))
    ;; If not, compute the kernel-info and place it in the cache.
    (let ((new-kernel-info (compute-kernel-info blueprint)))
      (bordeaux-threads:with-lock-held ((multicore-backend-kernel-info-lock backend))
        ;; Check once more whether another thread has computed the kernel
        ;; info in the meantime.  If so, discard ours and return that one.
        (multiple-value-bind (kernel-info presentp)
            (gethash blueprint cache)
          (when presentp (return-from kernel-info kernel-info)))
        ;; If there is still no kernel info in the cache, we install ours.
        (setf (gethash blueprint cache)
              new-kernel-info)
        new-kernel-info))))

(defun compute-kernel-info (blueprint)
  (make-kernel-info
   :compiled-kernel (compile nil (petalisp.ir:translate-blueprint blueprint))
   ;; TODO compute the task size in a sensible way.
   :task-size 2000))

(defun stuck-kernels (request)
  (let ((kernels '()))
    (petalisp.ir:map-kernels
     (lambda (kernel)
       (when (eq (kernel-data-state (petalisp.ir:kernel-data kernel)) :executing)
         (push kernel kernels)))
     (request-ir-roots request))
    kernels))
