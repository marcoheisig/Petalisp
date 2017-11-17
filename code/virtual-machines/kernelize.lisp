;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; Kernelization is the process of breaking a data flow specification into
;;; a set of executable parts. The data flow specification is given by a
;;; set of graph roots (usually the arguments passed to SCHEDULE) and forms
;;; a DAG (directed acyclic graph).
;;;
;;; The following algorithm partitions such a DAG into as large as possible
;;; kernels. All nodes of a kernel, up to the beginning of the next kernels
;;; form a tree, i.e. they have exactly one successor. Later, this allows
;;; to build a corresponding s-expression to actually evaluate each kernel.
;;;
;;; The result of kernelization is a set of immediate results, linked by
;;; kernels. The original DAG is not mutated in the process.
;;;
;;; The first step in the algorithm is the creation of a *USE-TABLE*, a
;;; hash table mapping each graph node to its successors. It would seem
;;; more efficient to track the users of each node at DAG construction time
;;; and avoid this indirection, but we are only interested in those users
;;; that are reachable via the given graph roots. This way the system
;;; implicitly eliminates dead code.
;;;
;;; Once the *USE-TABLE* is populated, the graph is traversed a second
;;; time, to generate a kernelized copy of it. A node with more than one
;;; user triggers the creation of a new intermediate result and one or more
;;; kernels. The hash table *KERNEL-TABLE* memoizes repeated calls to
;;; KERNELIZE-NODE.

(defun kernelize-graph (graph-roots)
  "Convert the data flow graph defined by GRAPH-ROOTS to an executable
   specification. Return a sequence of immediate values, each with a
   (possibly empty) set of kernels and dependencies."
  (let ((table (make-hash-table :test #'eq))
        (graph-roots (ensure-sequence graph-roots)))
    ;; step 1 - define a mapping from nodes to immediate values
    (labels ((register (node)
               (setf (gethash node table)
                     (corresponding-immediate node))
               (values))
             (register-root (node)
               (unless (immediate? node)
                 (register node)))
             (traverse (node)
               (if (or (< (refcount node) 2)
                       (immediate? node))
                   (traverse-inputs node)
                   (multiple-value-bind (value recurring)
                       (gethash node table)
                     (cond ((not recurring)
                            (setf (gethash node table) nil)
                            (traverse-inputs node))
                           ((and recurring (not value))
                            (register node))))))
             (traverse-inputs (node)
               (dolist (input (inputs node))
                 (traverse input))))
      ;; explicitly register all non-immediate graph roots, because they
      ;; are tree roots regardless of their type or refcount
      (map nil #'register-root graph-roots)
      ;; now process the entire graph recursively
      (map nil #'traverse graph-roots))
    ;; step 2 - derive the kernels of each immediate
    (labels
        ((kernelize-hash-table-entry (tree-root target)
           ;; TABLE has an entry for all nodes that are potential kernel
           ;; targets (i.e. their refcount is bigger than 1). But only
           ;; those nodes with a non-NIL target actually need to be
           ;; kernelized, the rest is skipped
           (when target
             (flet ((leaf-function (node)
                      (cond
                        ;; the root is never a leaf
                        ((eq node tree-root) nil)
                        ;; all immediates are leaves
                        ((immediate? node) node)
                        ;; skip the table lookup when the refcount is small
                        ((< (refcount node) 2) nil)
                        ;; otherwise check the table
                        (t (values (gethash node table))))))
               (declare (dynamic-extent #'leaf-function))
               (let ((kernels (subgraph-kernels target tree-root #'leaf-function)))
                 (setf (kernels target) kernels))))))
      (maphash #'kernelize-hash-table-entry table))
    (map 'vector
         (lambda (node)
           (if (immediate? node)
               node
               (gethash node table)))
         graph-roots)))
