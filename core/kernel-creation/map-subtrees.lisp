;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/kernel-creation/map-subtrees
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/kernel)
  (:export
   #:map-subtrees))

(in-package :petalisp/core/kernel-creation/map-subtrees)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finding Critical Nodes
;;;
;;; Critical nodes are those nodes/data-structures that must reside in main
;;; memory in order to avoid redundant evaluation. There are three criteria
;;; to determine whether a node is a critical node:
;;;
;;; 1. The node is one of the root nodes.
;;; 2. The node is referenced by multiple other nodes.
;;; 3. The node is the target of a broadcasting reference.

;;; The critical node table serves two purposes. The primary purpose is
;;; used to store one entry for each critical node, whose value is the
;;; newly created corresponding immediate node. The secondary purpose is to
;;; determine whether a potentially critical node (as determined by its
;;; reference count) is indeed used more than once. To do so, whenever a
;;; potentially critical node is encountered, it is registered in the table
;;; with a value of NIL. On a second encounter, the value is changed to the
;;; corresponding immediate node.
(defvar *critical-node-table* nil)

(defun process-critical-node (node)
  (when (not (nth-value 1 (gethash node *critical-node-table*)))
    (setf (gethash node *critical-node-table*)
          (corresponding-immediate node))
    (process-node-inputs node)))

(defun process-potentially-critical-node (node)
  (if (not (> (refcount node) 1))
      (process-node-inputs node)
      (multiple-value-bind (value recurring-p)
          (gethash node *critical-node-table*)
        (cond
          ;; First encounter
          ((not recurring-p)
           (setf (gethash node *critical-node-table*) nil)
           (process-node-inputs node))
          ;; Second encounter
          ((not value)
           (setf (gethash node *critical-node-table*)
                 (corresponding-immediate node)))))))

(defun process-node-inputs (node)
  (if (typep node 'reference)
      ;; Turn targets of broadcasting references (those that are not
      ;; invertible) into critical nodes.
      (if (not (invertible-transformation-p (transformation node)))
          (process-critical-node (input node))
          (process-potentially-critical-node (input node)))
      (loop for input in (inputs node) do
        (process-potentially-critical-node input))))

(defmacro with-critical-node-table ((graph-roots) &body body)
  (once-only (graph-roots)
    `(let ((*critical-node-table* (make-hash-table :test #'eq)))
       (map nil #'process-critical-node ,graph-roots)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Walking Subtrees

(defun lookup (node)
  (if (typep node 'immediate)
      (values node)
      (values (gethash node *critical-node-table*))))

(defun map-subtrees (subtree-fn graph-roots)
  "Invoke SUBTREE-FN on each subtree in the graph spanned by the supplied
GRAPH-ROOTS. For each subtree, SUBTREE-FN receives the following arguments:

1. The target immediate
2. The root of the tree in the data flow graph
3. A function, mapping each tree leaf to its corresponding immediate

Return the sequence of immediates corresponding to the GRAPH-ROOTS."
  (with-critical-node-table (graph-roots)
    (with-hash-table-iterator (generator *critical-node-table*)
      (loop
        (multiple-value-bind (more? tree-root corresponding-immediate) (generator)
          (cond ((not more?) (return))
                ;; Skip entries with NIL value
                ((not corresponding-immediate))
                ;; Skip immediate nodes
                ((typep tree-root 'immediate))
                ;; Each remaining key is the root of a non-empty subtree.
                (t (dx-flet ((leaf-function (node)
                               ;; the root is never a leaf
                               (if (eq node tree-root)
                                   nil
                                   (lookup node))))
                     (funcall subtree-fn corresponding-immediate tree-root #'leaf-function)))))))
    (map 'vector #'lookup graph-roots)))
