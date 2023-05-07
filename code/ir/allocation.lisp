;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; During the allocation phase, we look at each action to an infant ---
;;; because only infants have to be actually executed in the end --- and then
;;; assign each chunk that is referenced by such a action an allocation.  Also,
;;; each chunk corresponding to a root or leaf buffer gets its own allocation.
;;; The assignment of allocations to chunks is made such that all children of
;;; an allocated chunk share the same allocation.

(defstruct (allocation
            (:predicate allocationp)
            (:constructor make-allocation))
  ;; The ntype of all elements in this allocation.
  (ntype nil :type typo:ntype :read-only t)
  ;; The shape of this allocation.
  (shape nil :type shape :read-only t))

(defun allocator (primogenitor-chunk-vector)
  "Returns, for a supplied vector of primogenitor chunks, a function that maps
each chunk therein, and all the children thereof, to a suitable allocation, or
to NIL in case the chunk doesn't have to be allocated at all."
  (declare (simple-vector primogenitor-chunk-vector))
  (let ((table (make-hash-table :test #'eq)))
    ;; Assign each chunk an integer that is the number of times it is being
    ;; referenced by an infant.
    (labels ((reference (chunk)
               (incf (gethash chunk table 0)))
             (scan-chunk (chunk)
               (if (not (chunk-split chunk))
                   (loop for action in (chunk-actions chunk) do
                     (mapc #'reference (action-source-chunks action)))
                   (let ((split (chunk-split chunk)))
                     (scan-chunk (split-left-child split))
                     (scan-chunk (split-right-child split)))))
             (scan-primogenitor-chunk (chunk)
               (unless (interior-buffer-p (chunk-buffer chunk))
                 (reference chunk))
               (scan-chunk chunk)))
      (map nil #'scan-primogenitor-chunk primogenitor-chunk-vector))
    ;; Now, in a second sweep, replace each table entry with a suitable
    ;; allocation.
    (labels ((allocate-chunk (chunk allocation)
               (setf (gethash chunk table) allocation)
               (when (chunk-split chunk)
                 (let ((split (chunk-split chunk)))
                   (allocate-chunk (split-left-child split) allocation)
                   (allocate-chunk (split-right-child split) allocation))))
             (scan-chunk (chunk)
               (let ((count (gethash chunk table 0)))
                 (if (zerop count)
                     (when (chunk-split chunk)
                       (let ((split (chunk-split chunk)))
                         (scan-chunk (split-left-child split))
                         (scan-chunk (split-right-child split))))
                     (allocate-chunk
                      chunk
                      (make-allocation
                       :shape (chunk-shape chunk)
                       :ntype (buffer-ntype (chunk-buffer chunk))))))))
      (map nil #'scan-chunk primogenitor-chunk-vector))
    (lambda (chunk)
      (values
       (gethash chunk table)))))
