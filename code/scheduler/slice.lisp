;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.scheduler)

;;; A slice is a set of kernels that can be executed in parallel.  The
;;; scheduler turns a data flow graph into a list of slices and executes
;;; them one after the other on the available resources.

(defstruct (slice
            (:predicate slicep)
            (:constructor %make-slice (allocations kernels deallocations active-buffers)))
  ;; A list of buffers that need to be allocated before executing this slice.
  (allocations '() :type list)
  ;; A list of kernels that constitute this slice.
  (kernels '() :type list)
  ;; A list of buffers that can be deallocated after executing this slice.
  (deallocations '() :type list)
  ;; An alist of buffers to kernels that have yet to read from them.
  (active-buffers '() :type list))

(defun make-slice (kernels active-buffers)
  (let ((active-buffers
          ;; We copy both the alist of buffers, and the list of kernels
          ;; therein, so that we can operate destructively on them.
          (copy-tree active-buffers)))
    (dolist (kernel kernels)
      (petalisp.ir:map-kernel-inputs
       (lambda (buffer)
         (let ((entry (assoc buffer active-buffers)))
           (if (not entry)
               (error "~@<The input ~S of the kernel ~S
                          has not yet been allocated~:@>"
                      buffer kernel)
               (setf (cdr entry)
                     (delete kernel (cdr entry))))))
       kernel))
    (let ((deallocations
            (loop for (buffer . kernels) in active-buffers
                  when (and (null kernels)
                            (petalisp.ir:buffer-reusablep buffer))
                    collect buffer))
          (active-buffers
            (delete-if #'null active-buffers :key #'cdr))
          (allocations '()))
      ;; Ensure that output buffers that are not yet part of the set of
      ;; active buffers are allocated and added to the set.
      (dolist (kernel kernels)
        (petalisp.ir:map-kernel-outputs
         (lambda (buffer)
           (unless (or (assoc buffer active-buffers)
                       (member buffer allocations))
             (push (cons buffer (petalisp.ir:buffer-outputs buffer)) active-buffers)
             (push buffer allocations)))
         kernel))
      (%make-slice allocations kernels deallocations active-buffers))))

(defun make-initial-slice (root-buffers)
  (let ((kernels '())
        (active-buffers '()))
    (petalisp.ir:map-buffers-and-kernels
     (lambda (buffer)
       (when (leaf-buffer-p buffer)
         (push (cons buffer (petalisp.ir:buffer-outputs buffer))
               active-buffers)))
     (lambda (kernel)
       (when (kernel-ready-p kernel)
         (push kernel kernels)))
     root-buffers)
    (make-slice kernels active-buffers)))

(defun kernel-ready-p (kernel)
  (block result
    (petalisp.ir:map-kernel-outputs
     (lambda (buffer)
       (petalisp.ir:map-buffer-inputs
        (lambda (kernel)
          (unless (leaf-kernel-p kernel)
            (return-from result nil)))
        buffer))
     kernel)
    t))

(defun leaf-kernel-p (kernel)
  (block result
    (petalisp.ir:map-kernel-inputs
     (lambda (buffer)
       (unless (leaf-buffer-p buffer)
         (return-from result nil)))
     kernel)
    t))

(defun leaf-buffer-p (buffer)
  (null
   (petalisp.ir:buffer-inputs buffer)))
