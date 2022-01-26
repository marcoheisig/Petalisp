;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.xmas-backend)

(defclass xmas-backend (backend)
  ((%memory-pool
    :initform (make-memory-pool)
    :initarg :memory-pool
    :type memory-pool
    :reader xmas-backend-memory-pool)
   (%worker-pool
    :initform (alexandria:required-argument :worker-pool)
    :initarg :worker-pool
    :type worker-pool
    :reader xmas-backend-worker-pool)
   (%compile-cache
    :initarg :compile-cache
    :initform (make-hash-table :test #'eq)
    :reader xmas-backend-compile-cache)))

(defun xmas-backend-number-of-threads (xmas-backend)
  (worker-pool-size
   (xmas-backend-worker-pool xmas-backend)))

(defun make-xmas-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads (integer 1))
  (make-instance 'xmas-backend
    :memory-pool (make-memory-pool)
    :worker-pool (make-worker-pool threads)))

(defmethod delete-backend ((xmas-backend xmas-backend))
  (call-next-method))

(defmethod backend-evaluator
    ((xmas-backend xmas-backend)
     (lazy-arrays list)
     (unknowns list))
  (let* ((worker-pool (xmas-backend-worker-pool xmas-backend))
         (number-of-threads (xmas-backend-number-of-threads xmas-backend))
         (root-buffers (ir-from-lazy-arrays lazy-arrays))
         (program (buffer-program (first root-buffers)))
         ;; A vector entries are the buffers corresponding to each supplied
         ;; unknown in the third argument of BACKEND-EVALUATOR.
         (unknown-buffer-vector (compute-unknown-buffer-vector program unknowns))
         ;; Compute a vector that maps buffer numbers to either arrays that
         ;; are the value of those buffers, the symbol :UNKNOWN if those
         ;; buffers correspond to unknowns, or null, otherwise.
         (storage-vector (compute-storage-vector program))
         ;; A list whose elements are lists of buffers that can safely
         ;; share their allocated memory.
         (allocations (compute-allocations program storage-vector))
         ;; A vector of lists of functions.
         (thread-fns (compute-thread-fns xmas-backend program number-of-threads)))
    (lambda (&rest arrays)
      ;; Create a copy of the storage vector and fully initialize it.
      (let ((storage-vector (copy-seq storage-vector)))
        (flet ((buffer-storage (buffer)
                 (svref storage-vector (buffer-number buffer)))
               ((setf buffer-storage) (value buffer)
                 (setf (svref storage-vector (buffer-number buffer))
                       value)))
          ;; Insert all the supplied arrays into the storage vector.
          (loop for array in arrays
                for buffer across unknown-buffer-vector
                do (unless (and (typep array 'simple-array)
                                (= (array-rank array)
                                   (shape-rank (buffer-shape buffer)))
                                (loop for range in (shape-ranges (buffer-shape buffer))
                                      for axis from 0
                                      always (= (range-size range)
                                                (array-dimension array axis))))
                     (error "Not a simple array of shape ~S: ~S"
                            (buffer-shape buffer) array))
                   (unless (petalisp.type-inference:ntype=
                            (petalisp.type-inference:array-element-ntype array)
                            (buffer-ntype buffer))
                     (error "Not an array of type ~S: ~S"
                            (array-element-type array)
                            array))
                   (setf (buffer-storage buffer)
                         array))
          ;; Insert storage for all the remaining buffers into the storage
          ;; vector.
          (loop for (buffer . more-buffers) in allocations do
            (let ((array (memory-pool-allocate
                          (xmas-backend-memory-pool xmas-backend)
                          (buffer-ntype buffer)
                          (shape-dimensions (buffer-shape buffer)))))
              (setf (buffer-storage buffer) array)
              (dolist (buffer more-buffers)
                (setf (buffer-storage buffer) array))))
          ;; Start working.
          (loop for index below number-of-threads do
            (worker-enqueue
             (worker-pool-worker worker-pool index)
             (let ((fns (svref thread-fns index)))
               (lambda ()
                 (loop for fn in fns do (funcall fn #'buffer-storage))))))
          ;; Wait for all workers to finish.
          (let ((lock (bordeaux-threads:make-lock))
                (cvar (bordeaux-threads:make-condition-variable))
                (counter number-of-threads))
            (loop for index below number-of-threads do
              (worker-enqueue
               (worker-pool-worker worker-pool index)
               (lambda ()
                 (bordeaux-threads:with-lock-held (lock)
                   (when (zerop (decf counter))
                     (bordeaux-threads:condition-notify cvar))))))
            (bordeaux-threads:with-lock-held (lock)
              (loop until (zerop counter) do
                (bordeaux-threads:condition-wait cvar lock))))
          ;; Return the results.
          (loop for root-buffer in root-buffers
                collect (buffer-storage root-buffer)))))))

(defun compute-unknown-buffer-vector (program unknowns)
  (let* ((number-of-unknowns (length unknowns))
         (unknown-buffer-vector (make-array number-of-unknowns :initial-element nil)))
    (loop for (buffer . lazy-array) in (program-leaf-alist program)
          for delayed-action = (lazy-array-delayed-action lazy-array)
          when (delayed-unknown-p delayed-action) do
            (let ((position (position lazy-array unknowns)))
              (if (not position)
                  (error "Reference to an unknown unknown: ~S"
                         lazy-array)
                  (setf (svref unknown-buffer-vector position)
                        buffer))))
    unknown-buffer-vector))

(defun compute-storage-vector (program)
  (let ((storage-vector (make-array (program-number-of-buffers program) :initial-element nil)))
    (loop for (buffer . lazy-array) in (program-leaf-alist program)
          for delayed-action = (lazy-array-delayed-action lazy-array)
          do (typecase delayed-action
               (delayed-array
                (setf (svref storage-vector (buffer-number buffer))
                      (delayed-array-storage delayed-action)))
               (delayed-unknown
                (setf (svref storage-vector (buffer-number buffer))
                      :unknown))))
    storage-vector))

(defun compute-allocations (program storage-vector)
  (let* ((allocations '())
         ;; A vector with one bit per buffer.  An entry of 0 means we
         ;; currently don't care about this buffer in terms of liveness and
         ;; allocation.  An entry of 1 means we do.
         (buffer-activep-vector
           (make-array (program-number-of-buffers program)
                       :element-type 'bit
                       :initial-element 0))
         ;; A vector with one entry per task.  It is used to store the
         ;; liveness of buffers at each task.
         (task-live-buffers-vector
           (make-array (program-number-of-tasks program)
                       :initial-element '())))
    (map-uninitialized-buffers
     (lambda (buffers)
       (if (= 1 (length buffers))
           ;; If there is just a single buffer of that shape and ntype,
           ;; there is no need to perform graph coloring.  We simply mark
           ;; it for allocation.
           (push buffers allocations)
           (progn
             ;; Mark the buffers as being active.
             (dolist (buffer buffers)
               (setf (aref buffer-activep-vector (buffer-number buffer)) 1))
             ;; Determine the active buffers that are live at each task.  We
             ;; exploit the fact that the tasks in the task vector are sorted in
             ;; depth-first dependency order, so a single scan from right to left
             ;; solves the data flow problem for each task correctly.
             (loop for index from (1- (program-number-of-tasks program)) downto 0
                   for task = (svref (program-task-vector program) index)
                   do (let ((live-buffers '()))
                        (flet ((collect (buffer)
                                 (when (plusp (aref buffer-activep-vector (buffer-number buffer)))
                                   (map-task-defined-buffers
                                    (lambda (defined-buffer)
                                      (when (eq defined-buffer buffer)
                                        (return-from collect)))
                                    task)
                                   (pushnew buffer live-buffers))))
                          ;; The live buffers at a task are those buffers that are
                          ;; live at any of its successors, plus those used by it,
                          ;; minus those defined by it.
                          (map-task-successors
                           (lambda (successor)
                             (mapc #'collect (svref task-live-buffers-vector (task-number successor))))
                           task)
                          (map-task-kernels
                           (lambda (kernel)
                             (map-kernel-inputs #'collect kernel))
                           task))
                        (setf (aref task-live-buffers-vector (task-number task))
                              live-buffers)))
             ;; Build the conflict graph of buffers that must not be allocated in
             ;; the same location.  That graph has an edge from each defined
             ;; buffer of that task to each live buffer at that task.
             (let ((cgraph (petalisp.utilities:make-cgraph)))
               (do-program-tasks (task program)
                 (do-task-defined-buffers (defined-buffer task)
                   (loop for live-buffer in (svref task-live-buffers-vector (task-number task)) do
                     (petalisp.utilities:cgraph-add-conflict cgraph defined-buffer live-buffer))
                   (do-task-defined-buffers (other-buffer task)
                     (when (< (buffer-number other-buffer) (buffer-number defined-buffer))
                       (petalisp.utilities:cgraph-add-conflict cgraph defined-buffer other-buffer)))))
               ;; Color that graph.  All buffers of the same color can be placed
               ;; in the same allocation.
               (loop for buffers across (petalisp.utilities:cgraph-coloring cgraph) do
                 (push buffers allocations)))
             ;; Finally, mark the buffers inactive again.
             (dolist (buffer buffers)
               (setf (aref buffer-activep-vector (buffer-number buffer)) 0)))))
     program storage-vector)
    allocations))

(defun map-uninitialized-buffers (function program storage-vector)
  (let ((buffers '()))
    (map-program-buffers
     (lambda (buffer)
       (when (null (svref storage-vector (buffer-number buffer)))
         (push buffer buffers)))
     program)
    (setf buffers (stable-sort buffers #'petalisp.type-inference:ntype< :key #'buffer-ntype))
    (setf buffers (stable-sort buffers #'shape< :key #'buffer-shape))
    (loop until (null buffers) do
      (let* ((buffer (first buffers))
             (shape (buffer-shape buffer))
             (ntype (buffer-ntype buffer))
             (last buffers))
        ;; Locate the last cons cell whose CAR is a buffer with the same
        ;; shape and ntype.
        (loop for cdr = (cdr last)
              while (consp cdr)
              while (let* ((other-buffer (car cdr))
                           (other-shape (buffer-shape other-buffer))
                           (other-ntype (buffer-ntype other-buffer)))
                      (and (shape-equal shape other-shape)
                           (petalisp.type-inference:ntype= ntype other-ntype)))
              do (setf last (cdr last)))
        ;; Destructively cut the list of buffers right after that last
        ;; cons.
        (let ((rest (cdr last)))
          (setf (cdr last) nil)
          (funcall function buffers)
          (setf buffers rest))))))

(defparameter *min-per-thread-cost* 10000)

(defstruct subtask
  (fn (alexandria:required-argument :fn)
   :type function)
  (cost (alexandria:required-argument :cost)
   :type unsigned-byte))

(defun compute-thread-fns (xmas-backend program number-of-threads)
  (let ((compiled-kernel-vector (make-array (program-number-of-kernels program)))
        (thread-fns (make-array number-of-threads :initial-element '())))
    ;; Compile all kernels.
    (do-program-kernels (kernel program)
      (let ((blueprint (kernel-blueprint kernel)))
        (setf (svref compiled-kernel-vector (kernel-number kernel))
              (alexandria:ensure-gethash
               blueprint
               (xmas-backend-compile-cache xmas-backend)
               (compile nil (translate-blueprint blueprint))))))
    ;; To compute the schedule for each thread, we exploit the fact that
    ;; all tasks in the task vector are already enumerated in a way that
    ;; satisfies all dependencies.  So all we have to do is convert each
    ;; task into items on the schedule and follow up with a barrier.
    (map-program-tasks
     (lambda (task)
       ;; Pick a buffer from that task and add all kernels writing to it to
       ;; the schedule.  Repeat for all buffers until all kernels have been
       ;; scheduled.  And don't forget to emit barriers where necessary -
       ;; there can be inter-task dependencies.
       (let ((scheduled-kernels '())
             (buffers '()))
         (map-task-defined-buffers (lambda (buffer) (push buffer buffers)) task)
         ;; Start with the buffer with the least depth to automatically get
         ;; the dependencies within the task right.
         (loop for buffer in (sort buffers #'< :key #'buffer-depth) do
           (let ((subtasks '()))
             (do-buffer-inputs (kernel buffer)
               (unless (member kernel scheduled-kernels)
                 (push kernel scheduled-kernels)
                 (let ((fn (svref compiled-kernel-vector (kernel-number kernel)))
                       (cost (kernel-cost kernel)))
                   (map-partitioned-iteration-space
                    (lambda (iteration-space)
                      (push
                       (make-subtask
                        :fn (lambda (buffer-storage-fn)
                              (funcall fn kernel iteration-space buffer-storage-fn))
                        :cost (* (shape-size iteration-space) cost))
                       subtasks))
                    (kernel-iteration-space kernel)
                    (ceiling *min-per-thread-cost* cost)))))
             (unless (null subtasks)
               (let ((partitioning (petalisp.utilities:karmarkar-karp subtasks number-of-threads :weight #'subtask-cost)))
                 (loop for partition across partitioning for index from 0 do
                   (loop for subtask in partition do
                     (push (subtask-fn subtask)
                           (svref thread-fns index)))
                   (push (lambda (_) (declare (ignore _)) (barrier))
                         (svref thread-fns index)))))))))
     program)
    (loop for index below number-of-threads do
      (setf (svref thread-fns index)
            (nreverse (svref thread-fns index))))
    thread-fns))

(defun map-partitioned-iteration-space (function iteration-space max-task-size)
  (declare (type shape iteration-space)
           (type unsigned-byte max-task-size))
  (if (<= (shape-size iteration-space) max-task-size)
      (funcall function iteration-space)
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
            (let ((shape1 (make-shape (append prefix (list lo) suffix)))
                  (shape2 (make-shape (append prefix (list hi) suffix))))
              (map-partitioned-iteration-space function shape1 max-task-size)
              (map-partitioned-iteration-space function shape2 max-task-size)))))))
