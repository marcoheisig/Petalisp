;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.scheduler)

;;; LAZY-ARRAYS - A list of lazy arrays that are the roots of the data flow
;;; graph that is to be scheduled.
;;;
;;; N-WORKERS - The number of available workers.
;;;
;;; COMPUTE-TASKS - A function that receives a list of tasks and sends them
;;; to the workers.
(defun schedule-on-workers (lazy-arrays n-workers compute-tasks)
  (multiple-value-bind (root-buffers leaf-buffers)
      (petalisp.ir:ir-from-lazy-arrays lazy-arrays)
    (petalisp.ir:normalize-ir root-buffers)
    (labels ((schedule-1 (slice)
               (let ((kernels (available-kernels slice)))
                 (if (null kernels)
                     (values)
                     (let ((next-slice (make-slice kernels slice)))
                       (funcall compute-tasks
                                (tasks-from-slice next-slice (range 0 (1- n-workers))))
                       (schedule-1 next-slice))))))
      (schedule-1 (make-initial-slice leaf-buffers)))))

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
          (petalisp.ir:map-kernel-inputs
           (lambda (buffer)
             (unless (assoc buffer active-buffers)
               (return)))
           kernel)
          (push kernel potentially-available-kernels))))
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
                    (return)))
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
