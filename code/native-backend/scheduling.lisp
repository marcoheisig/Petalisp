;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defstruct (allocation
            (:predicate allocationp)
            (:constructor make-allocation))
  ;; The exponent of the first power of two larger than the size of the
  ;; allocation.
  (exponent nil :type unsigned-byte :read-only t)
  ;; An integer that identifies the worker that owns this allocation.
  (worker-id nil :type worker-id)
  ;; An integer that denotes the color of the allocation.  Within a worker, all
  ;; allocations with the same color use the same memory region.
  (color nil :type unsigned-byte :read-only t))

(defstruct (view
            (:predicate viewp)
            (:constructor make-view))
  ;; The number of elements referenced by the view.
  (size nil :type fixnum :read-only t)
  ;; The number of elements between the start of the allocation and the first
  ;; element of this view.
  (offset nil :type fixnum :read-only t)
  ;; A vector whose Ith entry is the number of elements to skip when changing
  ;; the Ith index component of an index by one.
  (strides nil :type (simple-array fixnum (*)) :read-only t)
  ;; The storage being viewed into.
  (storage nil :type storage :read-only t))

(defstruct copy
  (iteration-space nil :type shape :read-only t)
  (source nil :type view)
  (target nil :type view))

(defstruct call
  (kfn nil :type function :read-only t)
  (iteration-space nil :type shape :read-only t)
  (targets nil :type (simple-array view (*)))
  (sources nil :type (simple-array view (*))))

(defstruct (action
            (:predicate work-item-p)
            (:constructor make-work-item))
  (copies (alexandria:required-argument :copies)
   :type list
   :read-only t)
  (calls (alexandria:required-argument :calls)
   :type list
   :read-only t))

(defun compute-program-schedule (program primogenitor-buffer-shard-vector)
  (let ((buffer-shard-action-table (make-hash-table :test #'eq))
        (kernel-shard-action-table (make-hash-table :test #'eq))
        (graph (petalisp.scheduling:make-graph)))
    ;; We exploit that DO-PROGRAM-BUFFERS traverses a program's buffers in an
    ;; order that respects all data dependencies.  This means that when
    ;; progressing a buffer, we can rely on the fact that all sources of all
    ;; kernels writing into that buffer have already been processed.
    (do-program-buffers (buffer program)
      (break "TODO"))))
