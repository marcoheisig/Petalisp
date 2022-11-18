;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

(defvar *ir-checker-table*)

(defvar *ir-checker-worklist*)

(defgeneric check-ir-node (node))

(defun check-ir-node-eventually (node)
  (unless (gethash node *ir-checker-table*)
    (push node *ir-checker-worklist*)))

(defun check-ir (&rest nodes)
  (let ((*ir-checker-table* (make-hash-table :test #'eq))
        (*ir-checker-worklist* nodes))
    (loop until (null *ir-checker-worklist*)
          do (check-ir-node (pop *ir-checker-worklist*)))))

(defun check-reverse-link (node map-object map-fn)
  (funcall
   map-fn
   (lambda (x)
     (when (eq x node)
       (return-from check-reverse-link)))
   map-object)
  (error "Missing reverse link for a ~S in ~S."
         node map-object))

(defun duplicates (list)
  (remove-duplicates
   (loop for (elt . rest) on list
         when (member elt rest)
           collect elt)))

(defmethod check-ir-node :around ((object t))
  (unless (gethash object *ir-checker-table*)
    (setf (gethash object *ir-checker-table*) t)
    (call-next-method)))

(defmethod check-ir-node ((program program))
  (declare (optimize (debug 3) (safety 3)))
  (let ((initial-task (program-initial-task program))
        (final-task (program-final-task program))
        (buffers (make-hash-table))
        (kernels (make-hash-table))
        (tasks (make-hash-table))
        (worklist '()))
    ;; Ensure that the program leaf alist is well formed.
    (loop for (buffer . lazy-array) in (program-leaf-alist program) do
      (assert (leaf-buffer-p buffer))
      (assert (lazy-array-p lazy-array))
      (assert (member buffer (task-defined-buffers initial-task)))
      (push buffer worklist))
    (loop for root-buffer in (program-root-buffers program) do
      (assert (root-buffer-p root-buffer))
      (assert (buffer-task root-buffer))
      (assert (member final-task (task-successors (buffer-task root-buffer))))
      (push root-buffer worklist))
    ;; Gather all kernels and buffers in the program.
    (loop until (null worklist) for node = (pop worklist) do
      (etypecase node
        (buffer
         (unless (gethash node buffers)
           (setf (gethash node buffers) t)
           (do-buffer-inputs (kernel node) (push kernel worklist))
           (do-buffer-outputs (kernel node) (push kernel worklist))))
        (kernel
         (unless (gethash node kernels)
           (setf (gethash node kernels) t)
           (do-kernel-inputs (buffer node) (push buffer worklist))
           (do-kernel-outputs (buffer node) (push buffer worklist))))))
    ;; Gather all tasks in the program.
    (setf (gethash final-task tasks) t)
    (loop for buffer being the hash-keys of buffers do
      (assert (buffer-task buffer))
      (setf (gethash (buffer-task buffer) tasks) t))
    ;; Now make sure that the mapping functions for program buffers,
    ;; program kernels, and program tasks work correctly.
    (do-program-buffers (buffer program)
      (ecase (gethash buffer buffers)
        ((nil)
         (error "DO-PROGRAM-BUFFERS visits the unreachable buffer ~S." buffer))
        ((t)
         (setf (gethash buffer buffers) :visited))
        ((:visited)
         (error "The buffer ~S is visited multiple times by DO-PROGRAM-BUFFERS." buffer))))
    (assert (= (hash-table-count buffers) (program-number-of-buffers program)))
    (do-program-kernels (kernel program)
      (ecase (gethash kernel kernels)
        ((nil)
         (error "DO-PROGRAM-KERNELS visits the unreachable kernel ~S." kernel))
        ((t)
         (setf (gethash kernel kernels) :visited))
        ((:visited)
         (error "The kernel ~S is visited multiple times by DO-PROGRAM-KERNELS." kernel))))
    (assert (= (hash-table-count kernels) (program-number-of-kernels program)))
    (do-program-tasks (task program)
      (ecase (gethash task tasks)
        ((nil)
         (error "DO-PROGRAM-TASKS visits the unreachable task ~S." task))
        ((t)
         (setf (gethash task tasks) :visited))
        ((:visited)
         (error "The task ~S is visited multiple times by DO-PROGRAM-TASKS." task))))
    (assert (= (hash-table-count tasks) (program-number-of-tasks program)))
    (mapc #'check-ir-node-eventually (program-root-buffers program))))

(defmethod check-ir-node ((buffer buffer))
  (declare (optimize (debug 3) (safety 3)))
  ;; Check that all load instructions are wired correctly.
  (loop for (kernel . load-instructions) in (buffer-readers buffer) do
    (check-ir-node-eventually kernel)
    (assert (null (duplicates load-instructions)))
    (loop for load-instruction in load-instructions do
      (check-ir-node-eventually load-instruction)
      (assert (eq (load-instruction-buffer load-instruction) buffer))
      (check-reverse-link load-instruction kernel #'map-kernel-load-instructions)))
  ;; Check that all store instructions are wired correctly.
  (let ((number-of-writes 0))
    (loop for (kernel . store-instructions) in (buffer-writers buffer) do
      (check-ir-node-eventually kernel)
      (assert (null (duplicates store-instructions)))
      (loop for store-instruction in store-instructions do
        (check-ir-node-eventually store-instruction)
        (assert (eq (store-instruction-buffer store-instruction) buffer))
        (check-reverse-link store-instruction kernel #'map-kernel-store-instructions)
        (incf number-of-writes
              (shape-size
               (transform-shape
                (kernel-iteration-space kernel)
                (store-instruction-transformation store-instruction))))))
    ;; Ensure that the buffer's task is well formed (if it is defined).
    (when (taskp (buffer-task buffer))
      (check-ir-node-eventually (buffer-task buffer)))
    ;; Ensure that all elements of the buffer are actually written to.
    (unless (leaf-buffer-p buffer)
      (assert (= number-of-writes (buffer-size buffer))))))

(defmethod check-ir-node ((kernel kernel))
  (declare (optimize (debug 3) (safety 3)))
  ;; Ensure that all load instructions are wired correctly.
  (loop for (buffer . stencils) in (kernel-sources kernel) do
    (loop for stencil in stencils do
      (let ((load-instructions (stencil-load-instructions stencil)))
        (check-ir-node-eventually buffer)
        (assert (null (duplicates load-instructions)))
        (loop for load-instruction in load-instructions do
          (check-ir-node-eventually load-instruction)
          (assert (eq (load-instruction-buffer load-instruction) buffer))
          (loop for offset across (transformation-offsets (load-instruction-transformation load-instruction))
                for center across (stencil-center stencil)
                do (assert (<= (abs (- center offset)) *stencil-max-radius*)))
          (check-reverse-link load-instruction buffer #'map-buffer-load-instructions)))))
  ;; Ensure that all store instructions are wired correctly.
  (loop for (buffer . store-instructions) in (kernel-targets kernel) do
    (check-ir-node-eventually buffer)
    (assert (null (duplicates store-instructions)))
    (loop for store-instruction in store-instructions do
      (check-ir-node-eventually store-instruction)
      (assert (eq (store-instruction-buffer store-instruction) buffer))
      (check-reverse-link store-instruction buffer #'map-buffer-store-instructions)))
  ;; Ensure that the kernel's task is well formed (if it is defined).
  (when (taskp (kernel-task kernel))
    (check-reverse-link kernel (kernel-task kernel) #'map-task-kernels)
    (check-ir-node-eventually (kernel-task kernel))))

(defmethod check-ir-node ((instruction instruction))
  (loop for (value-n . other-instruction) in (instruction-inputs instruction) do
    (assert (instructionp other-instruction))
    (check-ir-node-eventually other-instruction)
    (assert (typep value-n 'unsigned-byte))
    (unless (zerop value-n)
      (assert (typep other-instruction 'call-instruction))
      (assert (< value-n (call-instruction-number-of-values other-instruction))))))

(defmethod check-ir-node :after ((store-instruction store-instruction))
  ;; It doesn't make sense to have multiple writes to the same memory
  ;; location.  If a kernel has such a store instruction, something must
  ;; have gone wrong elsewhere.
  (assert (transformation-invertiblep
           (store-instruction-transformation store-instruction))))

(defmethod check-ir-node ((task task))
  (declare (optimize (debug 3) (safety 3)))
  ;; Ensure that all kernels writing to a buffer with task T also have the
  ;; task T.
  (loop for buffer in (task-defined-buffers task) do
    (assert (eq (buffer-task buffer) task))
    (check-ir-node-eventually buffer)
    (do-buffer-inputs (kernel buffer)
      (assert (eq (kernel-task kernel) task))))
  ;; Ensure that all buffers written to by a kernel with task T have the
  ;; task T.
  (loop for kernel in (task-kernels task) do
    (assert (eq (kernel-task kernel) task))
    (check-ir-node-eventually kernel)
    (do-kernel-outputs (buffer kernel)
      (assert (eq (buffer-task buffer) task))))
  ;; Ensure that a buffer that is used by a kernel in T and that depends on
  ;; a buffer in T is also in T.
  (let* ((max-depth (reduce #'max (task-defined-buffers task)
                            :key #'buffer-depth
                            :initial-value 0))
         (event-horizon (event-horizon (task-defined-buffers task) max-depth)))
    (do-task-kernels (kernel task)
      (do-kernel-inputs (buffer kernel)
        (when (member buffer event-horizon)
          (assert (eq (buffer-task buffer) task))))))
  ;; Ensure that the tasks of all kernels that read from a buffer defined
  ;; by this task are successors of this task.
  (do-task-defined-buffers (buffer task)
    (do-buffer-outputs (kernel buffer)
      (let ((successor (kernel-task kernel)))
        (unless (eq successor task)
          (assert (member successor (task-successors task)))
          (assert (member task (task-predecessors successor))))))))
