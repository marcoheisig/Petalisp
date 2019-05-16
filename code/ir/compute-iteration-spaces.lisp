;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; This file defines the function COMPUTE-ITERATION-SPACES that, when
;;; invoked on a node and in the context of a valid *BUFFER-TABLE*, will
;;; compute a partitioning of the shape of that node into one or more
;;; subspaces.  These subspaces are chosen such that any path from a
;;; particular subspace upwards passes through exactly one input of each
;;; fusion node until reaching another node with an entry in the buffer
;;; table.
;;;
;;; We compute this partitioning by recursively traversing all nodes in the
;;; current subtree, while tracking both the current iteration space, and a
;;; mapping from the current iteration space to the iteration space of the
;;; root.  Each of these recursive functions returns a boolean, indicating
;;; whether any of the inputs of the current node, or any of the inputs
;;; thereof, is a fusion node.  When visiting any fusion node, each input
;;; that yields a true value after recursive processing is projected back
;;; to the iteration space of the root node and added to the list of
;;; iteration spaces that will be returned in the end.

(defvar *root*)

(defvar *register-iteration-space*)

(defun register-iteration-space (iteration-space)
  (funcall *register-iteration-space* iteration-space))

(defgeneric compute-iteration-spaces (root-node))

(defgeneric compute-iteration-spaces-aux (node iteration-space transformation))

(defmethod compute-iteration-spaces ((root lazy-array))
  (let* ((*root* root)
         (iteration-spaces '())
         (*register-iteration-space*
           (lambda (iteration-space)
             (push (cons iteration-space nil)
                   iteration-spaces))))
    (compute-iteration-spaces-aux
     root
     (shape root)
     (identity-transformation (rank root)))
    (or iteration-spaces
        (list (cons (shape root) nil)))))

(defun reduction-range (reduction)
  (first (ranges (shape (first (inputs reduction))))))

(defmethod compute-iteration-spaces ((root reduction))
  (let* ((*root* root)
         (iteration-spaces '())
         (*register-iteration-space*
           (lambda (iteration-space)
             (push (multiple-value-bind (iteration-space reduction-range)
                       (shrink-shape iteration-space)
                     (cons iteration-space reduction-range))
                   iteration-spaces))))
    (compute-iteration-spaces-aux
     root
     (enlarge-shape (shape root) (reduction-range root))
     (identity-transformation (1+ (rank root))))
    (or iteration-spaces
        (list (cons (shape root) (reduction-range root))))))

(defmethod compute-iteration-spaces-aux :around
    ((node lazy-array) (iteration-space shape) (transformation transformation))
  (if (eq node *root*)
      (call-next-method)
      (if (nth-value 1 (gethash node *buffer-table*))
          nil
          (call-next-method))))

(defmethod compute-iteration-spaces-aux
    ((fusion fusion) (iteration-space shape) (transformation transformation))
  ;; Check whether any inputs are free of fusion nodes.  If so, push an
  ;; iteration space.
  (loop for input in (inputs fusion) do
    (let ((subspace (set-intersection iteration-space (shape input))))
      ;; If the input is unreachable, we do nothing.
      (unless (set-emptyp subspace)
        ;; If the input contains fusion nodes, we also do nothing.
        (unless (compute-iteration-spaces-aux input subspace transformation)
          ;; We have an outer fusion.  This means we have to add a new
          ;; iteration space, which we obtain by projecting the current
          ;; iteration space to the coordinate system of the root.
          (register-iteration-space
           (transform subspace (invert-transformation transformation)))))))
  t)

(defmethod compute-iteration-spaces-aux
    ((reference reference) (iteration-space shape) (transformation transformation))
  (compute-iteration-spaces-aux
   (input reference)
   (transform
    (set-intersection iteration-space (shape reference))
    (transformation reference))
   (compose-transformations (transformation reference) transformation)))

(defmethod compute-iteration-spaces-aux
    ((reduction reduction) (iteration-space shape) (transformation transformation))
  (loop for input in (inputs reduction)
          thereis
          (compute-iteration-spaces-aux input iteration-space transformation)))

(defmethod compute-iteration-spaces-aux
    ((application application) (iteration-space shape) (transformation transformation))
  (loop for input in (inputs application)
          thereis
          (compute-iteration-spaces-aux input iteration-space transformation)))

(defmethod compute-iteration-spaces-aux
    ((immediate immediate) (iteration-space shape) (transformation transformation))
  (error "This is a default method that should never be reached."))
