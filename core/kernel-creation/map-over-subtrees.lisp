;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/map-over-subtrees
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/kernel)
  (:export
   #:map-over-subtrees))

(in-package :petalisp/core/kernel-creation/map-over-subtrees)

(defun map-over-subtrees (subtree-fn graph-roots)
  "Invoke SUBTREE-FN on each subtree in the graph spanned by the supplied
GRAPH-ROOTS. For each subtree, SUBTREE-FN receives the following arguments:

1. The target immediate
2. The root of the tree in the data flow graph
3. A function, mapping each tree leaf to its corresponding immediate

Return the sequence of immediates corresponding to the GRAPH-ROOTS."
  (let ((critical-node-table (make-hash-table :test #'eq)))
    ;; Naively, CRITICAL-NODE-TABLE would simply contain an entry for each
    ;; critical node, mapping it to its corresponding immediate value. But
    ;; since there is initially some uncertainty about which nodes are
    ;; critical, the table will also contain an entry for each node with a
    ;; refcount of two or higher, but with a value of NIL. Furthermore,
    ;; immediates are not necessarily placed in the table, since they are
    ;; always critical and only map to themselves.
    (labels ((register-critical-node (node)
               (unless (gethash node critical-node-table)
                 (setf (gethash node critical-node-table)
                       (corresponding-immediate node))
                 (recurse-into node)
                 (values nil)))
             (register-potentially-critical-node (node)
               (multiple-value-bind (value recurring)
                   (gethash node critical-node-table)
                 (unless value
                   (cond
                     ((not recurring)
                      (setf (gethash node critical-node-table) nil)
                      (recurse-into node))
                     (recurring
                      (setf (gethash node critical-node-table)
                            (corresponding-immediate node))
                      (values nil))))))
             (recurse-into (node)
               (typecase node
                 ;; deliberately ignore immediates
                 (reduction (traverse (input node) nil))
                 (reference (traverse (input node) (not (invertible? (transformation node)))))
                 ((or application fusion)
                  (let ((reductions
                          (loop for input in (inputs node)
                                count (traverse input nil))))
                    (case reductions
                      (0 (values nil))
                      (1 (values t))
                      (otherwise (register-critical-node node)))))))
             (traverse (node critical?)
               (cond
                 (critical?             (register-critical-node node))
                 ((> (refcount node) 1) (register-potentially-critical-node node))
                 (t                     (recurse-into node)))))
      (map nil #'register-critical-node graph-roots))
    ;; now call SUBTREE-FN for each subtree
    (labels ((lookup (node)
               (if (typep node 'immediate)
                   node
                   (values (gethash node critical-node-table))))
             (process-hash-table-entry (tree-root target)
               (when (and target (not (typep tree-root 'immediate)))
                 (flet ((leaf-function (node)
                          ;; the root is never a leaf
                          (unless (eq node tree-root)
                            (lookup node))))
                   (declare (dynamic-extent #'leaf-function))
                   (funcall subtree-fn target tree-root #'leaf-function)))))
      (maphash #'process-hash-table-entry critical-node-table)
      (map 'vector #'lookup graph-roots))))

