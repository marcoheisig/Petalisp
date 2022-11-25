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

(defun compile-kernel (kernel xmas-backend)
  (let ((blueprint (kernel-blueprint kernel)))
    (alexandria:ensure-gethash
     blueprint
     (xmas-backend-compile-cache xmas-backend)
     (compile nil (translate-blueprint blueprint)))))

(defun make-xmas-backend (&key (threads (petalisp.utilities:number-of-cpus)))
  (check-type threads (integer 1))
  (make-instance 'xmas-backend
    :memory-pool (make-memory-pool)
    :worker-pool (make-worker-pool threads)))

(defmethod delete-backend ((xmas-backend xmas-backend))
  (call-next-method))

(defmethod backend-evaluator
    ((xmas-backend xmas-backend)
     (unknowns list)
     (lazy-arrays list))
  (multiple-value-call (evaluator-template (length lazy-arrays) (length unknowns))
    (evaluator-template-arguments xmas-backend lazy-arrays unknowns)))

(let ((cache (make-hash-table))
      ;; Threshold below which a program's buffer storage vector is
      ;; allocated on the stack.
      (stack-allocation-limit 40))
  (defun evaluator-template (number-of-results number-of-arguments)
    (alexandria:ensure-gethash
     number-of-arguments
     (alexandria:ensure-gethash number-of-results cache (make-hash-table))
     (let ((results (result-variables number-of-results))
           (arguments (argument-variables number-of-arguments)))
       (compile
        nil
        `(lambda (backend number-of-buffers process-results process-arguments allocate run deallocate)
           (if (< number-of-buffers ,stack-allocation-limit)
               (lambda (,@results ,@arguments)
                 (let ((storage-vector (make-array ,stack-allocation-limit :initial-element nil)))
                   (declare (dynamic-extent storage-vector))
                   (funcall process-results backend storage-vector ,@results)
                   (funcall process-arguments backend storage-vector ,@arguments)
                   (funcall allocate backend storage-vector)
                   (unwind-protect (funcall run backend storage-vector)
                     (funcall deallocate backend storage-vector))))
               (lambda (,@results ,@arguments)
                 (let ((storage-vector (make-array number-of-buffers :initial-element nil)))
                   (funcall process-results backend storage-vector ,@results)
                   (funcall process-arguments backend storage-vector ,@arguments)
                   (funcall allocate backend storage-vector)
                   (unwind-protect (funcall run backend storage-vector)
                     (funcall deallocate backend storage-vector)))))))))))

(defun generate-variable (prefix integer)
  (intern
   (with-output-to-string (stream)
     (loop for char across (string prefix) do
       (write-char char stream))
     (format stream "~D" integer))
   #.*package*))

(defun result-variables (n)
  (loop for i below n collect (generate-variable "DST" i)))

(defun argument-variables (n)
  (loop for i below n collect (generate-variable "SRC" i)))

(defun evaluator-template-arguments (xmas-backend lazy-arrays unknowns)
  (let* ((root-buffers (ir-from-lazy-arrays lazy-arrays))
         (program (buffer-program (first root-buffers))))
    (multiple-value-bind (allocator deallocator) (allocator-and-deallocator program)
      (values
       xmas-backend
       (program-number-of-buffers program)
       (result-processor root-buffers)
       (argument-processor program unknowns)
       allocator
       (runner program root-buffers xmas-backend)
       deallocator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Argument Processing

(defun result-processor (root-buffers)
  (let* ((number-of-results (length root-buffers)))
    (lambda (xmas-backend storage-vector &rest arrays)
      (assert (= (length arrays) number-of-results))
      (let ((memory-pool (xmas-backend-memory-pool xmas-backend)))
        (loop for buffer in root-buffers
              for array in arrays do
                (if (null array)
                    (allocate-buffer buffer storage-vector memory-pool)
                    (bind-buffer buffer storage-vector array)))))))

(defun argument-processor (program unknowns)
  (let* ((number-of-unknowns (length unknowns))
         (missing-unknowns number-of-unknowns)
         (unknown-buffer-vector (make-array number-of-unknowns :initial-element nil))
         (buffer-array-alist '()))
    (loop for (buffer . lazy-array) in (program-leaf-alist program) do
      (typecase (lazy-array-delayed-action lazy-array)
        (delayed-unknown
         (let ((position (position lazy-array unknowns)))
           (cond ((null position)
                  (error "Reference to an unknown unknown: ~S"
                         lazy-array))
                 ((not (null (svref unknown-buffer-vector position)))
                  (error "Multiple buffers per unknown."))
                 (t
                  (decf missing-unknowns)
                  (setf (svref unknown-buffer-vector position)
                        buffer)))))
        (delayed-array
         (push (cons buffer (delayed-array-storage (lazy-array-delayed-action lazy-array)))
               buffer-array-alist))))
    (assert (zerop missing-unknowns))
    (lambda (xmas-backend storage-vector &rest arrays)
      (declare (ignore xmas-backend))
      (declare (simple-vector storage-vector))
      (assert (= (length arrays) number-of-unknowns))
      (loop for array in arrays
            for buffer across unknown-buffer-vector do
              (bind-buffer buffer storage-vector array))
      (loop for (buffer . array) in buffer-array-alist do
        (setf (svref storage-vector (buffer-number buffer))
              array)))))

(defun bind-buffer (buffer storage-vector array)
  (declare (buffer buffer))
  (declare (simple-vector storage-vector))
  (unless (and (typep array 'simple-array)
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
  (setf (svref storage-vector (buffer-number buffer))
        array))

(defun allocate-buffer (buffer storage-vector memory-pool)
  (declare (buffer buffer))
  (declare (simple-vector storage-vector))
  (declare (memory-pool memory-pool))
  (unless (svref storage-vector (buffer-number buffer))
    (setf (svref storage-vector (buffer-number buffer))
          (memory-pool-allocate
           memory-pool
           (buffer-ntype buffer)
           (shape-dimensions (buffer-shape buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Memory Management

(defun allocator-and-deallocator (program)
  (let ((allocations (petalisp.ir:compute-program-buffer-coloring program)))
    (values
     ;; The allocator.
     (lambda (xmas-backend storage-vector)
       (declare (simple-vector storage-vector))
       (let ((memory-pool (xmas-backend-memory-pool xmas-backend)))
         (loop for (buffer . other-buffers) in allocations do
           (allocate-buffer buffer storage-vector memory-pool)
           (dolist (other-buffer other-buffers)
             (setf (svref storage-vector (buffer-number other-buffer))
                   (svref storage-vector (buffer-number buffer))))))
       (loop for index below (program-number-of-buffers program) do
         (unless (arrayp (svref storage-vector index))
           (error "Failed to allocate buffer ~S."
                  (program-buffer program index)))))
     ;; The deallocator.
     (lambda (xmas-backend storage-vector)
       (let ((memory-pool (xmas-backend-memory-pool xmas-backend)))
         (loop for (buffer . other-buffers) in allocations do
           (when (interior-buffer-p buffer)
             (memory-pool-free memory-pool (svref storage-vector (buffer-number buffer))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execution

(defparameter *min-per-thread-cost* 20000)

(defstruct subtask
  (kernel (alexandria:required-argument :kernel)
   :type kernel)
  (iteration-space (alexandria:required-argument :iteration-space)
   :type shape)
  (fn (alexandria:required-argument :fn)
   :type function)
  (cost (alexandria:required-argument :cost)
   :type unsigned-byte))

(defun runner (program root-buffers xmas-backend)
  ;; Start by computing a schedule, which is a list of vectors.  Each
  ;; vector has one element per worker, which is a list of subtasks.  Each
  ;; vector in the schedule is executed by running each list of subtasks on
  ;; the corresponding worker and then executing a barrier.
  (let ((number-of-threads (xmas-backend-number-of-threads xmas-backend))
        (schedule '()))
    ;; To compute the schedule, we exploit the fact that all tasks in the
    ;; task vector are already enumerated in a way that satisfies all
    ;; dependencies.  So all we have to do is convert each task in that
    ;; order.
    (do-program-tasks (task program)
      ;; Pick a buffer from that task and add all kernels writing to it to
      ;; the schedule.  Repeat for all buffers until all kernels have been
      ;; scheduled.
      (let ((scheduled-kernels '())
            (buffers '()))
        (do-task-defined-buffers (buffer task)
          (push buffer buffers))
        ;; Start with the buffer with the least depth to automatically get
        ;; the dependencies within the task right.
        (loop for buffer in (sort buffers #'< :key #'buffer-depth) do
          (let* ((ntype (buffer-ntype buffer))
                 ;; Most CPU's allow atomic writes of individual bytes, but
                 ;; not of individual half-bytes, quarter-bytes, or bits.
                 ;; This means that we have to ensure that all writes in a
                 ;; multi-threaded environment obey a certain alignment.
                 (alignment (ceiling 8 (petalisp.type-inference:ntype-bits ntype)))
                 (subtasks '()))
            (do-buffer-inputs (kernel buffer)
              (unless (member kernel scheduled-kernels)
                (push kernel scheduled-kernels)
                (let ((fn (compile-kernel kernel xmas-backend))
                      (cost-per-element (petalisp.ir::kernel-highest-instruction-number kernel)))
                  (map-partitioned-iteration-space
                   (lambda (iteration-space)
                     (push
                      (make-subtask
                       :kernel kernel
                       :iteration-space iteration-space
                       :fn fn
                       :cost (* (shape-size iteration-space) cost-per-element))
                      subtasks))
                   (kernel-iteration-space kernel)
                   (ceiling *min-per-thread-cost* cost-per-element)))))
            (unless (null subtasks)
              ;; If the total cost of all subtasks is less than a certain
              ;; threshold, schedule them all on worker zero.  Else,
              ;; distribute the them evenly across all workers.
              (push
               (if (or (< (reduce #'+ subtasks :key #'subtask-cost :initial-value 0) *min-per-thread-cost*)
                       ;; TODO right now, we disable multi-threading for
                       ;; non-trivial alignments.
                       (/= 1 alignment))
                   (let ((vector (make-array number-of-threads :initial-element '())))
                     (setf (elt vector 0) subtasks)
                     vector)
                   (petalisp.utilities:karmarkar-karp subtasks number-of-threads :weight #'subtask-cost))
               schedule))))))
    (setf schedule (nreverse schedule))
    (lambda (xmas-backend storage-vector)
      (let* ((worker-pool (xmas-backend-worker-pool xmas-backend))
             (semaphore (bordeaux-threads:make-semaphore))
             (serious-condition nil))
        (loop for index below number-of-threads do
          (worker-enqueue
           (worker-pool-worker worker-pool index)
           (lambda ()
             (execute-schedule
              schedule
              storage-vector
              (lambda () serious-condition)
              (lambda (c)
                (setf serious-condition c)
                (bordeaux-threads:signal-semaphore semaphore))))))
        ;; It is enough to have worker zero signal completion, because we
        ;; emit a barrier after each step.
        (worker-enqueue
         (worker-pool-worker worker-pool 0)
         (lambda ()
           (bordeaux-threads:signal-semaphore semaphore)))
        (bordeaux-threads:wait-on-semaphore semaphore)
        (when serious-condition (error serious-condition))
        (values-list
         (loop for root-buffer in root-buffers
               collect (svref storage-vector (buffer-number root-buffer))))))))

(defun execute-schedule (schedule storage-vector serious-condition signal-serious-condition)
  (declare (simple-vector storage-vector))
  (let ((id (worker-id *worker*)))
    (loop for vector of-type simple-vector in schedule until (funcall serious-condition) do
      (barrier)
      (handler-case
          (loop for subtask in (svref vector id) do
            (funcall (subtask-fn subtask)
                     (subtask-kernel subtask)
                     (subtask-iteration-space subtask)
                     (lambda (buffer) (svref storage-vector (buffer-number buffer)))))
        (serious-condition (c)
          (funcall signal-serious-condition c))))
    ;; Finally, synchronize all workers.
    (barrier)))

(defun map-partitioned-iteration-space (function iteration-space max-task-size)
  (declare (type shape iteration-space)
           (type unsigned-byte max-task-size))
  (if (<= (shape-size iteration-space) max-task-size)
      (funcall function iteration-space)
      (let* ((ranges (shape-ranges iteration-space))
             (rank (shape-rank iteration-space))
             (max-axis 0)
             (max-range (first ranges)))
        (loop for range in (rest ranges) and axis from 1
              when (> (if (= axis (1- rank))
                          ;; Penalize splitting the innermost range.
                          (floor (range-size range) 4)
                          (range-size range))
                      (range-size max-range))
                do (setf max-axis axis max-range range))
        (let ((prefix (subseq ranges 0 max-axis))
              (suffix (subseq ranges (1+ max-axis))))
          (multiple-value-bind (lo hi) (split-range max-range)
            (let ((shape1 (make-shape (append prefix (list lo) suffix)))
                  (shape2 (make-shape (append prefix (list hi) suffix))))
              (map-partitioned-iteration-space function shape1 max-task-size)
              (map-partitioned-iteration-space function shape2 max-task-size)))))))
