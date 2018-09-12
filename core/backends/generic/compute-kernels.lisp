;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Subtree Fragments to Kernels
;;;
;;; The goal is to convert a given subtree of a data flow graph to a list
;;; of kernels.  The subtree is delimited by nodes that have a
;;; corresponding entry in the buffer table.  By choosing the iteration
;;; space of our kernels appropriately, we can eliminate all fusion nodes
;;; in the subtree.
;;;
;;; The algorithm consists of two phases.  In the first phase, we compute a
;;; partitioning of the shape of the root into multiple iteration spaces.
;;; These spaces are chosen such that their union is the shape of the root,
;;; but such that each iteration space selects only a single input of each
;;; encountered fusion node.  In the second phase, each iteration space is
;;; used to create one kernel and its body.  The body of a kernel is an
;;; s-expression describing the interplay of applications, reductions and
;;; references.

(defmethod compute-kernels ((root strided-array) (backend backend))
  (loop for iteration-space in (compute-iteration-spaces root)
        collect
        (let* ((body (compute-kernel-body root iteration-space))
               (kernel (make-kernel iteration-space body backend)))
          (loop for input in (inputs kernel) do
            (push kernel (outputs input)))
          (loop for output in (outputs kernel) do
            (push kernel (inputs output))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Iteration Spaces

(defvar *kernel-iteration-spaces*)

(defun compute-iteration-spaces (root)
  (let ((*kernel-iteration-spaces* '()))
    (compute-iteration-spaces-aux
     root
     (shape root)
     (make-identity-transformation (dimension root)))
    *kernel-iteration-spaces*))

;;; Return a boolean indicating whether any of the inputs of NODE, or any
;;; of the inputs thereof, is a fusion node.  Furthermore, whenever NODE is
;;; a fusion node, push a new iteration space for each input that contains
;;; no further fusion nodes.
(defgeneric compute-iteration-spaces-aux
    (node iteration-space transformation))

(defmethod compute-iteration-spaces-aux :around
    ((strided-array strided-array)
     (iteration-space shape)
     (transformation transformation))
  (if (nth-value (gethash strided-array *buffer-table*) 1)
      nil
      (call-next-method)))

(defmethod compute-iteration-spaces-aux
    ((fusion fusion)
     (iteration-space shape)
     (transformation transformation))
  (loop for input in (inputs fusion) do
    (let ((subspace (set-intersection iteration-space (shape input))))
      ;; If the input is unreachable, we do nothing.
      (unless (set-emptyp subspace)
        ;; If the input contains fusion nodes, we also do nothing.
        (unless (compute-iteration-spaces-aux input subspace transformation)
          ;; We have an outer fusion.  This means we have to add a new
          ;; iteration space, which we obtain by projecting the current
          ;; iteration space to the coordinate system of the root.
          (push (transform iteration-space (invert-transformation transformation))
                *kernel-iteration-spaces*))))))

(defmethod compute-iteration-spaces-aux
    ((reference reference)
     (iteration-space shape)
     (transformation transformation))
  (compute-iteration-spaces-aux
   (input reference)
   (set-intersection iteration-space (shape reference))
   (compose-transformations (transformation reference) transformation)))

(defmethod compute-iteration-spaces-aux
    ((reduction reduction)
     (iteration-space shape)
     (transformation transformation))
  (let ((iteration-space (enlarge-shape iteration-space (reduction-range reduction)))
        (transformation (enlarge-transformation transformation 1 0)))
    (loop for input in (inputs reduction)
            thereis
            (compute-iteration-spaces-aux input iteration-space transformation))))

(defmethod compute-iteration-spaces-aux
    ((application application)
     (iteration-space shape)
     (transformation transformation))
  (loop for input in (inputs application)
          thereis
          (compute-iteration-spaces-aux input iteration-space transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the Kernel Body

(defun compute-kernel-body (root iteration-space)
  `(pset ,(gethash root *buffer-table*)
         ,(compute-kernel-body-aux
           root
           root
           iteration-space
           (make-identity-transformation (dimension root)))))

(defgeneric compute-kernel-body-aux
    (root node iteration-space transformation))

;; Check whether we are dealing with a leaf, i.e., a node that has a
;; corresponding entry in the buffer table and is not the root node.  If
;; so, return a reference to that buffer.
(defmethod compute-kernel-body-aux :around
    ((root strided-array)
     (node strided-array)
     (iteration-space shape)
     (transformation transformation))
  (unless (set-emptyp iteration-space)
    (if (eq root node)
        (call-next-method)
        (multiple-value-bind (buffer buffer-p)
            (gethash node *buffer-table*)
          (if (not buffer-p)
              (call-next-method)
              `(pref ,buffer ,transformation))))))

(defmethod compute-kernel-body-aux
    ((root strided-array)
     (application application)
     (iteration-space shape)
     (transformation transformation))
  `(papply
    ,(operator application)
    ,.(loop for input in (inputs application)
            collect (compute-kernel-body-aux
                     root
                     input
                     iteration-space
                     transformation))))

(defmethod compute-kernel-body-aux
    ((root strided-array)
     (reduction reduction)
     (iteration-space shape)
     (transformation transformation))
  `(preduce
    ,(operator reduction)
    ,.(let ((iteration-space (enlarge-shape iteration-space (reduction-range reduction)))
            (transformation (enlarge-transformation transformation 1 0)))
        (loop for input in (inputs reduction)
              collect (compute-kernel-body-aux
                       root
                       input
                       iteration-space
                       transformation)))))

(defmethod compute-kernel-body-aux
    ((root strided-array)
     (reference reference)
     (iteration-space shape)
     (transformation transformation))
  (compute-kernel-body-aux
   root
   (input reference)
   (set-intersection iteration-space (shape reference))
   (compose-transformations (transformation reference) transformation)))

(defmethod compute-kernel-body-aux
    ((root strided-array)
     (fusion fusion)
     (iteration-space shape)
     (transformation transformation))
  (let ((input (find iteration-space (inputs fusion)
                     :key #'shape
                     :test #'set-intersectionp)))
    (compute-kernel-body-aux
     root
     input
     (set-intersection iteration-space (shape input))
     transformation)))
