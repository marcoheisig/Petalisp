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
     (lazy-arrays list)
     (unknowns list))
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

(defun map-program-buffer-groups (function program)
  (let ((buffers '()))
    (do-program-buffers (buffer program)
      (unless (leaf-buffer-p buffer)
        (push buffer buffers)))
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

(defmacro do-program-buffer-groups ((buffers program &optional result) &body body)
  (check-type buffers symbol)
  `(block nil
     (map-program-buffer-groups (lambda (buffers) ,@body) ,program)
     ,result))

(defun allocator-and-deallocator (program)
  (let* (;; A list of lists of buffers.  Each list of buffers contains at
         ;; most one root buffer, which is then the first buffer of that
         ;; list.
         (allocations '())
         ;; A vector with one bit per buffer.  An entry of 0 means we
         ;; currently don't care about this buffer in terms of liveness and
         ;; allocation.  An entry of 1 means we do.
         (buffer-activep-vector
           (make-array (program-number-of-buffers program)
                       :element-type 'bit
                       :initial-element 0))
         ;; A vector with one list of buffers per task.  It is used to
         ;; store the liveness of buffers at each task.
         (task-live-buffers-vector
           (make-array (program-number-of-tasks program)
                       :initial-element '())))
    (do-program-buffer-groups (buffers program)
      (cond
        ;; If there is just a single buffer of that shape and ntype, there
        ;; is no need to perform graph coloring.  We simply mark it for
        ;; allocation.
        ((= 1 (length buffers))
         (push buffers allocations))
        (t
         ;; Mark the buffers as being active.
         (dolist (buffer buffers)
           (setf (aref buffer-activep-vector (buffer-number buffer)) 1))
         ;; Determine the active buffers that are live at each task.  We
         ;; exploit the fact that the tasks in the task vector are sorted
         ;; in depth-first dependency order, so a single scan from right to
         ;; left solves the data flow problem for each task correctly.
         (loop for index from (1- (program-number-of-tasks program)) downto 0
               for task = (svref (program-task-vector program) index)
               do (let ((live-buffers '()))
                    (flet ((collect (buffer)
                             (when (plusp (aref buffer-activep-vector (buffer-number buffer)))
                               (do-task-defined-buffers (defined-buffer task)
                                 (when (eq defined-buffer buffer)
                                   (return-from collect)))
                               (pushnew buffer live-buffers))))
                      ;; The live buffers at a task are those buffers that
                      ;; are live at any of its successors, plus those used
                      ;; by it, minus those defined by it.
                      (do-task-successors (successor task)
                        (mapc #'collect (svref task-live-buffers-vector (task-number successor))))
                      (do-task-kernels (kernel task)
                        (map-kernel-inputs #'collect kernel)))
                    (setf (aref task-live-buffers-vector (task-number task))
                          live-buffers)))
         ;; Build the conflict graph of buffers that must not be allocated
         ;; in the same location.  That graph has an edge from each defined
         ;; buffer of that task to each live buffer at that task.
         (let ((cgraph (petalisp.utilities:make-cgraph)))
           (do-program-tasks (task program)
             (do-task-defined-buffers (defined-buffer task)
               (when (plusp (aref buffer-activep-vector (buffer-number defined-buffer)))
                 (loop for live-buffer in (svref task-live-buffers-vector (task-number task)) do
                   (petalisp.utilities:cgraph-add-conflict cgraph defined-buffer live-buffer))
                 (do-task-defined-buffers (other-buffer task)
                   (when (plusp (aref buffer-activep-vector (buffer-number other-buffer)))
                     (when (< (buffer-number other-buffer) (buffer-number defined-buffer))
                       (petalisp.utilities:cgraph-add-conflict cgraph defined-buffer other-buffer)))))))
           ;; Color that graph.  All buffers of the same color can be
           ;; placed in the same allocation.
           (loop for buffers across (petalisp.utilities:cgraph-coloring cgraph) do
             ;; Ensure that if there is a root buffer among that list
             ;; of buffers, it occurs first.
             (loop for cons on buffers do
               (when (root-buffer-p (car cons))
                 (rotatef (car cons) (car buffers))))
             (push buffers allocations)))
         ;; Finally, mark the buffers inactive again.
         (dolist (buffer buffers)
           (setf (aref buffer-activep-vector (buffer-number buffer)) 0)))))
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
         (assert (arrayp (svref storage-vector index)))))
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
        (do-task-defined-buffers (buffer task) (push buffer buffers))
        ;; Start with the buffer with the least depth to automatically get
        ;; the dependencies within the task right.
        (loop for buffer in (sort buffers #'< :key #'buffer-depth) do
          (let ((subtasks '()))
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
              (push (petalisp.utilities:karmarkar-karp subtasks number-of-threads :weight #'subtask-cost)
                    schedule))))))
    (setf schedule (nreverse schedule))
    (lambda (xmas-backend storage-vector)
      (let* ((worker-pool (xmas-backend-worker-pool xmas-backend))
             (semaphore (bordeaux-threads:make-semaphore))
             (serious-condition nil)
             (thunk
               (lambda ()
                 (execute-schedule
                  schedule
                  storage-vector
                  (lambda () serious-condition)
                  (lambda (c)
                    (setf serious-condition c)
                    (bordeaux-threads:signal-semaphore semaphore))))))
        (loop for index below number-of-threads do
          (worker-enqueue (worker-pool-worker worker-pool index) thunk))
        (worker-enqueue
         (worker-pool-worker worker-pool 0)
         (lambda () (bordeaux-threads:signal-semaphore semaphore)))
        (bordeaux-threads:wait-on-semaphore semaphore)
        (when serious-condition (error serious-condition))
        (loop for root-buffer in root-buffers
              collect (svref storage-vector (buffer-number root-buffer)))))))

(defun execute-schedule (schedule storage-vector serious-condition signal-serious-condition)
  (declare (simple-vector storage-vector))
  (let ((id (worker-id *worker*)))
    (loop for vector in schedule until (funcall serious-condition) do
      (handler-case
          (loop for subtask in (svref vector id) do
            (funcall (subtask-fn subtask)
                     (subtask-kernel subtask)
                     (subtask-iteration-space subtask)
                     (lambda (buffer) (svref storage-vector (buffer-number buffer)))))
        (serious-condition (c)
          (funcall signal-serious-condition c)))
      (barrier))))

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
