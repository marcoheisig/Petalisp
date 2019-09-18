;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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

(defun make-slice (kernels parent)
  (let ((active-buffers
          ;; We copy both the alist of buffers, and the list of kernels
          ;; therein, so that we can operate destructively later on.
          (copy-tree
           (slice-active-buffers parent))))
    (dolist (kernel kernels)
      (petalisp.ir:map-kernel-inputs
       (lambda (buffer)
         (let ((entry (assoc buffer active-buffers)))
           (if (not entry)
               (error "~@<The input ~S of the kernel ~S
                          has not yet been allocated~:@>"
                      buffer kernel)
               (setf (cdr entry) (delete kernel (cdr entry))))))
       kernel))
    (let ((deallocations
            (loop for (buffer . kernels) in active-buffers
                  when (null kernels)
                    collect buffer))
          (active-buffers
            (delete-if #'null active-buffers :key #'cdr))
          (allocations '()))
      ;; Ensure that output buffers that are not yet part of the set active
      ;; buffers are allocated and added to the set.
      (dolist (kernel kernels)
        (petalisp.ir:map-kernel-outputs
         (lambda (buffer)
           (unless (or (assoc buffer active-buffers)
                       (member buffer allocations))
             (push (cons buffer (petalisp.ir:buffer-outputs buffer)) active-buffers)
             (push buffer allocations)))
         kernel))
      (%make-slice allocations kernels deallocations active-buffers))))

(defun make-initial-slice (leaf-buffers)
  (%make-slice
   '()
   '()
   '()
   (loop for leaf-buffer in leaf-buffers
         collect
         (cons leaf-buffer (petalisp.ir:buffer-outputs leaf-buffer)))))
