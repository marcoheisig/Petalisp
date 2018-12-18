;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are strided arrays, into an analogous graph, whose nodes are buffers
;;; and kernels.  Kernels and buffers alternate, such that the inputs and
;;; outputs of a kernel are always buffers, and such that the inputs and
;;; outputs of a buffer are always kernels.
;;;
;;; The IR conversion algorithm proceeds along the following steps:
;;;
;;; 1. A hash table is created that maps certain strided arrays to buffers
;;;    of the same size and element type.  This table is constructed such
;;;    that any subgraph without these nodes is a tree and contains no
;;;    reduction nodes.
;;;
;;; 2. Each root of a subtree from step 1 is turned into one or more
;;;    kernels.  All fusion nodes in the tree are eliminated by choosing
;;;    the iteration space of the kernels appropriately.
;;;
;;; 3. All buffers are updated to contain a list of kernels that read to
;;;    them or write from them.

(defun ir-from-strided-arrays (strided-arrays backend)
  (let ((*buffer-table* (compute-buffer-table strided-arrays backend)))
    ;; Now create a list of kernels for each entry in the buffer table.
    (loop for root being each hash-key of *buffer-table* do
      (let ((kernels (compute-kernels root backend)))
        ;; Update the inputs and outputs of all buffers to match the inputs
        ;; and outputs of the corresponding kernels.
        (loop for kernel in kernels do
          (loop for load in (loads kernel) do
            (pushnew kernel (outputs (buffer load))))
          (loop for store in (stores kernel) do
            (pushnew kernel (inputs (buffer store)))))))
    ;; Finally, return the buffers corresponding to the root nodes.
    (loop for strided-array in strided-arrays
          collect
          (let ((entry (gethash strided-array *buffer-table*)))
            (if (eq entry 'range-immediate-placeholder)
                (make-buffer strided-array backend)
                entry)))))

(defvar *kernel-root*)

;;; We compute a partitioning of the shape of the root into multiple
;;; iteration spaces.  These spaces are chosen such that their union is the
;;; shape of the root, but such that each iteration space selects only a
;;; single input of each encountered fusion node. Each such iteration space
;;; is used to create one kernel.
(defun compute-kernels (root backend)
  (unless (immediatep root)
    (let ((*kernel-root* root))
      (loop for (iteration-space . reduction-range) in (compute-iteration-spaces root)
            collect
            (compute-kernel root iteration-space reduction-range backend)))))

(defun compute-kernel (root iteration-space reduction-range backend)
  (let* ((root-rank (rank root))
         (transformation (identity-transformation root-rank)))
    (with-instruction-numbering
      (multiple-value-bind (value loads)
          (compute-kernel-body root iteration-space transformation)
        (make-kernel
         backend
         :iteration-space iteration-space
         :reduction-range reduction-range
         :loads loads
         :stores (list (make-instance 'store-instruction
                         :value value
                         :buffer (gethash root *buffer-table*)
                         :transformation transformation)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the Kernel Body

(defvar *loads*)

(defun compute-kernel-body (root iteration-space transformation)
  (let ((*loads* '()))
    (values (compute-value root iteration-space transformation) *loads*)))

;;; Return the 'value' of ROOT for a given point in the iteration space of
;;; the kernel, i.e., return a cons cell whose cdr is an instruction and
;;; whose car is an integer denoting which of the N values of the
;;; instructions is referenced.
(defgeneric compute-value (node iteration-space transformation))

;; Check whether we are dealing with a leaf, i.e., a node that has a
;; corresponding entry in the buffer table and is not the root node.  If
;; so, return a reference to that buffer.
(defmethod compute-value :around
    ((node strided-array)
     (iteration-space shape)
     (transformation transformation))
  ;; The root node has an entry in the buffer table, yet we do not want to
  ;; treat it as a leaf node.
  (if (eq node *kernel-root*)
      (call-next-method)
      (multiple-value-bind (buffer buffer-p)
          (gethash node *buffer-table*)
        (if (not buffer-p)
            (call-next-method)
            (if (eq buffer 'range-immediate-placeholder)
                (cons 0 (make-instance 'iref-instruction
                          :transformation transformation))
                (let ((load (make-instance 'load-instruction
                              :transformation transformation
                              :buffer buffer)))
                  (push load *loads*)
                  (cons 0 load)))))))

(defmethod compute-value
    ((application application)
     (iteration-space shape)
     (transformation transformation))
  (cons (value-n application)
        (make-instance 'call-instruction
          :operator (operator application)
          :arguments
          (loop for input in (inputs application)
                collect
                (compute-value input iteration-space transformation)))))

(defmethod compute-value
    ((reduction reduction)
     (iteration-space shape)
     (transformation transformation))
  (let* ((shape (shape (first (inputs reduction)))))
    (cons (value-n reduction)
          (make-instance 'reduce-instruction
            :operator (operator reduction)
            :arguments
            (loop for input in (inputs reduction)
                  collect
                  (compute-value
                   input
                   shape
                   (enlarge-transformation transformation 1 0)))))))

(defmethod compute-value
    ((reference reference)
     (iteration-space shape)
     (transformation transformation))
  (compute-value
   (input reference)
   (transform
    (set-intersection iteration-space (shape reference))
    (transformation reference))
   (compose-transformations (transformation reference) transformation)))

(defmethod compute-value
    ((fusion fusion)
     (iteration-space shape)
     (transformation transformation))
  (let ((input (find iteration-space (inputs fusion)
                     :key #'shape
                     :test #'set-intersectionp)))
    (compute-value
     input
     (set-intersection iteration-space (shape input))
     transformation)))

(defmethod compute-value
    ((strided-array strided-array)
     (iteration-space shape)
     (transformation transformation))
  (error "Can't IR convert ~S" strided-array))
