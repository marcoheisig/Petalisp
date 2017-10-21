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
  (let ((table (make-hash-table :test #'eq))
        (roots (ensure-sequence graph-roots))
        (immediates (fvector)))
    ;; step 1 - define a mapping from nodes to immediate values
    (labels ((register (node)
               (setf (gethash node table)
                     (corresponding-immediate node))
               (values))
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
      (map nil #'register roots)
      (map nil #'traverse roots))
    ;; step 2 - derive the kernels of each immediate
    (labels
        ((kernelize-hash-table-entry (graph-root target)
           (when target
             (flet ((leaf-function (node)
                      (cond
                        ((eq node graph-root) nil)
                        ((immediate? node) node)
                        ((< (refcount node) 2) nil)
                        (t (values (gethash node table))))))
               (let ((kernels (kernelize-tree target graph-root #'leaf-function)))
                 (setf (kernels target) kernels)
                 (fvector-push target immediates))))))
      (maphash #'kernelize-hash-table-entry table))
    ;; step 3 - determine the dependencies of each immediate
    immediates))

(defun kernelize-tree (target graph-root leaf-function)
  (let (kernels)
    (map-recipes
     (lambda (recipe iteration-space ranges sources)
       (push (make-instance 'kernel
               :iteration-space iteration-space
               :recipe recipe
               :target target
               :sources sources)
             kernels))
     graph-root
     :leaf-function leaf-function)
    kernels))
