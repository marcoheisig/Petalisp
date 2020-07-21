;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are lazy arrays, into an analogous graph, whose nodes are buffers and
;;; kernels.  Kernels and buffers alternate, such that the inputs and
;;; outputs of a kernel are always buffers, and such that the inputs and
;;; outputs of a buffer are always kernels.
;;;
;;; The IR conversion algorithm proceeds along the following steps:
;;;
;;; 1. A hash table is created that maps certain strided arrays to buffers
;;;    of the same size and element type.  This table is constructed such
;;;    that any subgraph without these nodes is a tree.
;;;
;;; 2. Each root of a subtree from step 1 is turned into one or more
;;;    kernels.  All fusion nodes in the tree are eliminated by choosing
;;;    the iteration space of the kernels appropriately.
;;;
;;; 3. All buffers are updated to contain a list of kernels that read to
;;;    them or write from them.

(defun ir-from-lazy-arrays (lazy-arrays)
  (with-layout-table (lazy-arrays)
    ;; Create a list of kernels for each entry in the buffer table.
    (maphash
     (lambda (lazy-array layout)
       (unless (immediatep lazy-array)
         (create-kernels lazy-array layout)))
     *layout-table*)
    ;; Finalize all layouts.
    (maphash
     (lambda (lazy-array layout)
       (declare (ignore lazy-array))
       (finalize-layout layout))
     *layout-table*)
    ;; Delete all buffers that are referenced zero times.
    (maphash
     (lambda (lazy-array layout)
       (declare (ignore lazy-array))
       (when (lazy-array-layout-p layout)
         (loop for (buffer . nil) in (layout-buffer-stores layout) do
           (unless (buffer-readers buffer)
             (delete-buffer buffer)))))
     *layout-table*)
    ;; Normalize the entire IR and return the root buffers.
    (let ((root-buffers
            (loop for lazy-array in lazy-arrays
                  collect
                  (layout-buffer (layout-table-entry lazy-array)))))
      (normalize-ir root-buffers)
      root-buffers)))

;;; We compute a partitioning of the shape of the root into multiple
;;; iteration spaces.  These spaces are chosen such that their union is the
;;; shape of the root, and such that each iteration space selects only a
;;; single input of each encountered fusion node.  Each such iteration
;;; space is used to create one kernel.
(defun create-kernels (root root-layout)
  (let ((*root* root))
    (map-iteration-spaces
     (lambda (iteration-space)
       (let ((kernel (compute-kernel root root-layout iteration-space)))
         (assign-instruction-numbers kernel)
         ;; Update the readers of all referenced buffers.
         (map-kernel-load-instructions
          (lambda (load-instruction)
            (let* ((buffer (load-instruction-buffer load-instruction))
                   (found (assoc kernel (buffer-readers buffer))))
              (etypecase found
                (null
                 (push (list kernel load-instruction)
                       (buffer-readers buffer)))
                (cons
                 (push load-instruction (cdr found))))))
          kernel)
         ;; Update the writers of all referenced buffers.
         (map-kernel-store-instructions
          (lambda (store-instruction)
            (let* ((buffer (store-instruction-buffer store-instruction))
                   (found (assoc kernel (buffer-writers buffer))))
              (etypecase found
                (null
                 (push (list kernel store-instruction)
                       (buffer-writers buffer)))
                (cons
                 (push store-instruction (cdr found))))))
          kernel)))
     root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing one Kernel

(defun compute-kernel (root root-layout iteration-space)
  (multiple-value-bind (store-instructions load-instructions)
      (compute-kernel-body root root-layout iteration-space)
    (make-kernel
     :iteration-space iteration-space
     :store-instructions store-instructions
     :load-instructions load-instructions)))

(defun compute-kernel-body (root root-layout iteration-space)
  (let* ((rank (shape-rank iteration-space))
         (transformation (identity-transformation rank))
         (*layout-buffer-loads* '()))
    (values
     (list
      (layout-store
       root-layout
       (compute-value root iteration-space transformation)
       iteration-space
       transformation))
     (petalisp.utilities:with-collectors ((load-instructions collect))
       (loop for (nil . buffer-loads) in *layout-buffer-loads* do
         (loop for (nil . loads) in buffer-loads do
           (mapc #'collect loads)))
       (load-instructions)))))

;;; Return the 'value' of ROOT for a given point in the iteration space of
;;; the kernel, i.e., return a cons cell whose cdr is an instruction and
;;; whose car is an integer denoting which of the N values of the
;;; instructions is referenced.
(defgeneric compute-value (node iteration-space transformation))

;; Check whether we are dealing with a leaf, i.e., a node that has a
;; corresponding entry in the buffer table and is not the root node.  If
;; so, return a reference to that buffer.
(defmethod compute-value :around
    ((node lazy-array)
     (iteration-space shape)
     (transformation transformation))
  ;; The root node always has an entry in the buffer table, yet we do not
  ;; want to treat it as a leaf node.
  (if (eq node *root*)
      (call-next-method)
      (let ((layout (layout-table-entry node)))
        (if (null layout)
            (call-next-method)
            (cons 0 (layout-load layout iteration-space transformation))))))

(defmethod compute-value
    ((lazy-map single-value-lazy-map)
     (iteration-space shape)
     (transformation transformation))
  (cons 0
        (make-single-value-call-instruction
         (operator lazy-map)
         (loop for input in (inputs lazy-map)
               collect
               (compute-value input iteration-space transformation)))))

(defmethod compute-value
    ((lazy-map multiple-value-lazy-map)
     (iteration-space shape)
     (transformation transformation))
  (cons (value-n lazy-map)
        (make-multiple-value-call-instruction
         (number-of-values lazy-map)
         (operator lazy-map)
         (loop for input in (inputs lazy-map)
               collect
               (compute-value input iteration-space transformation)))))

(defmethod compute-value
    ((lazy-reshape lazy-reshape)
     (iteration-space shape)
     (transformation transformation))
  (compute-value
   (input lazy-reshape)
   (transform
    (shape-intersection iteration-space (shape lazy-reshape))
    (transformation lazy-reshape))
   (compose-transformations
    (transformation lazy-reshape)
    transformation)))

(defmethod compute-value
    ((lazy-fuse lazy-fuse)
     (iteration-space shape)
     (transformation transformation))
  (let ((input (find iteration-space (inputs lazy-fuse)
                     :key #'shape
                     :test #'shape-intersectionp)))
    (compute-value
     input
     (shape-intersection iteration-space (shape input))
     transformation)))

(defmethod compute-value
    ((lazy-array lazy-array)
     (iteration-space shape)
     (transformation transformation))
  (error "Can't IR convert ~S" lazy-array))
