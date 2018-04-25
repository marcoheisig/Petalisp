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

(defun map-subtrees (subtree-fn graph-roots)
  "Invoke SUBTREE-FN on each subtree in the graph spanned by the supplied
GRAPH-ROOTS. For each subtree, SUBTREE-FN receives the following arguments:

1. The target immediate
2. The root of the tree in the data flow graph
3. A function, mapping each tree leaf to its corresponding immediate

Return the sequence of immediates corresponding to the GRAPH-ROOTS."
  (let ((critical-node-table (make-hash-table :test #'eq)))
    ;; Naively, CRITICAL-NODE-TABLE would simply contain an entry for each
    ;; critical node, mapping it to its corresponding immediate value.  But
    ;; since there is initially some uncertainty about which nodes are
    ;; critical, the table will also contain an entry for each node with a
    ;; refcount of two or higher, but with a hash table value of NIL.
    ;; Furthermore, immediates are not necessarily placed in the table,
    ;; since they are always critical and only map to themselves.
    (labels ((register-critical-node (node)
               (when (not (gethash node critical-node-table))
                 (setf (gethash node critical-node-table)
                       (corresponding-immediate node))
                 (visit-inputs node)))
             (maybe-register-critical-node (node)
               (multiple-value-bind (value recurring-p)
                   (gethash node critical-node-table)
                 (when (not value)
                   (cond
                     ((not recurring-p)
                      (setf (gethash node critical-node-table) nil)
                      (visit-inputs node))
                     (recurring-p
                      (setf (gethash node critical-node-table)
                            (corresponding-immediate node)))))))
             (visit-inputs (node)
               ;; The targets of broadcasting references are
               ;; unconditionally turned into critical nodes.
               (if (typep node 'reference)
                   (if (invertible-transformation-p (transformation node))
                      (visit (input node))
                      (register-critical-node (input node)))
                   (mapc #'visit (inputs node))))
             (visit (node)
               (if (> (refcount node) 1)
                   (maybe-register-critical-node node)
                   (visit-inputs node))))
      (map nil #'register-critical-node graph-roots))
    ;; now call SUBTREE-FN for each subtree
    (labels ((lookup (node)
               (if (typep node 'immediate)
                   node
                   (values (gethash node critical-node-table))))
             (process-hash-table-entry (tree-root corresponding-immediate)
               (when (and corresponding-immediate
                          (not (typep tree-root 'immediate)))
                 (dx-flet ((leaf-function (node)
                             ;; the root is never a leaf
                             (unless (eq node tree-root)
                               (lookup node))))
                   (funcall subtree-fn corresponding-immediate tree-root #'leaf-function)))))
      (maphash #'process-hash-table-entry critical-node-table)
      (map 'vector #'lookup graph-roots))))

