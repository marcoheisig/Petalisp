;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

(defvar *ir-checker-table*)

(defvar *ir-checker-worklist*)

(defvar *ir-checker-kernel-data-type*)

(defvar *ir-checker-buffer-data-type*)

(defgeneric check-ir-node (node))

(defun check-ir-node-eventually (node)
  (unless (gethash node *ir-checker-table*)
    (push node *ir-checker-worklist*)))

(defun check-ir (nodes &key (kernel-data-type 't) (buffer-data-type 't))
  (let ((*ir-checker-table* (make-hash-table :test #'eq))
        (*ir-checker-worklist* nodes)
        (*ir-checker-kernel-data-type* kernel-data-type)
        (*ir-checker-buffer-data-type* buffer-data-type))
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

(defmethod check-ir-node ((buffer buffer))
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
    (unless (buffer-storage buffer)
      (assert (= number-of-writes (buffer-size buffer))))
    ;; Ensure that the buffer's data slot has a sane value.
    (assert (typep (buffer-data buffer) *ir-checker-buffer-data-type*))))

(defmethod check-ir-node ((kernel kernel))
  ;; Ensure that all load instructions are wired correctly.
  (loop for (buffer . load-instructions) in (kernel-sources kernel) do
    (check-ir-node-eventually buffer)
    (assert (null (duplicates load-instructions)))
    (loop for load-instruction in load-instructions do
      (check-ir-node-eventually load-instruction)
      (assert (eq (load-instruction-buffer load-instruction) buffer))
      (check-reverse-link load-instruction buffer #'map-buffer-load-instructions)))
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
    (check-ir-node-eventually (kernel-task kernel)))
  ;; Ensure that the kernel's data slot has a sane value.
  (assert (typep (kernel-data kernel) *ir-checker-kernel-data-type*)))

(defmethod check-ir-node ((instruction instruction))
  (loop for (value-n . other-instruction) in (instruction-inputs instruction) do
    (assert (instructionp other-instruction))
    (check-ir-node-eventually other-instruction)
    (assert (typep value-n 'unsigned-byte))
    (unless (zerop value-n)
      (assert (typep other-instruction 'call-instruction))
      (assert (< value-n (call-instruction-number-of-values other-instruction))))))

(defmethod check-ir-node ((task task))
  (loop for buffer in (task-defined-buffers task) do
    (check-ir-node-eventually buffer)
    (assert (eq (buffer-task buffer) task))
    ;; Ensure that all kernels writing to one of the buffers defined by a
    ;; task belong to that task.
    (map-buffer-inputs
     (lambda (kernel)
       (assert (eq (kernel-task kernel) task)))
     buffer))
  ;; Ensure that the task's buffers are live.
  (let ((live-buffers '()))
    (loop for kernel in (task-kernels task) do
      (check-ir-node-eventually kernel)
      (map-kernel-inputs
       (lambda (buffer)
         (pushnew buffer live-buffers))
       kernel)
      (map-kernel-outputs
       (lambda (buffer)
         (pushnew buffer live-buffers))
       kernel))
    (assert (null (set-difference live-buffers (task-live-buffers task))))))
