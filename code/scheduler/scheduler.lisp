;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.scheduler)

;;; LAZY-ARRAYS - A list of lazy arrays that are the roots of the data flow
;;; graph that is to be scheduled.
;;;
;;; N-WORKERS - The number of available workers.
;;;
;;; COMPUTE-TASKS - A function that receives a list of tasks and sends them
;;; to the workers.
(defun schedule-on-workers (lazy-arrays n-workers enqueue-tasks barrier allocate deallocate)
  (let ((root-buffers (petalisp.ir:ir-from-lazy-arrays lazy-arrays)))
    (petalisp.ir:normalize-ir root-buffers)
    ;; Now comes the actual scheduling.
    (let ((current-slice (make-initial-slice root-buffers)))
      (mapc allocate (slice-allocations current-slice))
      (loop
        (funcall enqueue-tasks (tasks-from-slice current-slice (range 0 (1- n-workers))))
        (let ((next-slice (compute-next-slice current-slice)))
          (funcall barrier)
          (mapc deallocate (slice-deallocations current-slice))
          (when (null next-slice)
            (return))
          (mapc allocate (slice-allocations next-slice))
          (setf current-slice next-slice))))
    ;; Return the results.
    (loop for root-buffer in root-buffers
          for lazy-array in lazy-arrays
          collect
          (if (immediatep lazy-array)
              lazy-array
              (let ((storage (petalisp.ir:buffer-storage root-buffer)))
                (assert (arrayp storage))
                (lazy-array storage))))))

;;; Return a suitable next slice, or NIL, if all work is done.
(defun compute-next-slice (slice)
  (let ((kernels (available-kernels slice)))
    (unless (null kernels)
      (make-slice kernels (slice-active-buffers slice)))))

;;; Compute all kernels whose target buffers are ready.  A buffer is ready
;;; if all kernels writing to it are ready.  A kernel is ready if it only
;;; reads from buffers that are part of the active-buffers of SLICE.
(defun available-kernels (slice)
  (let ((active-buffers (slice-active-buffers slice))
        (potentially-available-kernels '()))
    ;; Step 1 - Pick all kernels that are reachable from the buffers in the
    ;; active-buffers of SLICE, and that have no input buffer that is not in
    ;; the active-buffers of SLICE.
    (loop for (nil . kernels) in active-buffers do
      (loop for kernel in kernels do
        (unless (member kernel potentially-available-kernels)
          (block skip-this-kernel
            (petalisp.ir:map-kernel-inputs
             (lambda (buffer)
               (unless (assoc buffer active-buffers)
                 (return-from skip-this-kernel)))
             kernel)
            (push kernel potentially-available-kernels)))))
    ;; Step 2 - Pick all buffers whose inputs are potentially available
    ;; kernels.
    (let ((available-buffers '()))
      (loop for kernel in potentially-available-kernels do
        (petalisp.ir:map-kernel-outputs
         (lambda (buffer)
           (unless (member buffer available-buffers)
             (block nil
               (petalisp.ir:map-buffer-inputs
                (lambda (kernel)
                  (unless (member kernel potentially-available-kernels)
                    ;; Should we encounter a kernel with zero inputs, we
                    ;; retroactively add it to the potentially available
                    ;; kernels.
                    (if (zerop (petalisp.ir:kernel-number-of-loads kernel))
                        (pushnew kernel potentially-available-kernels)
                        (return))))
                buffer)
               (push buffer available-buffers))))
         kernel))
      ;; Step 3 - Filter all kernels that are potentially available for
      ;; kernels that are actually available.
      (flet ((available-kernel-p (kernel)
               (block nil
                 (prog1 t
                   (petalisp.ir:map-kernel-outputs
                    (lambda (buffer)
                      (unless (member buffer available-buffers)
                        (return nil)))
                    kernel)))))
        (delete-if-not #'available-kernel-p potentially-available-kernels)))))
