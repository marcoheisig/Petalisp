;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; Kernelization is the process of breaking a data flow specification into
;;; a set of executable parts. The data flow specification is given by a
;;; set of graph roots (usually the arguments passed to SCHEDULE) and forms
;;; a DAG (directed acyclic graph).
;;;
;;; The following algorithm cuts such a DAG into as large as possible
;;; kernels. All nodes of a kernel, up to the beginning of the next kernels
;;; form a tree, i.e. they have exactly one successor. Later, this allows
;;; to build a corresponding s-expression to actually evaluate each kernel.
;;;
;;; The result of kernelization is a set of kernels. The original DAG is
;;; not mutated in the process.
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
;;; user triggers the creation of a new kernel. The hash table
;;; *KERNEL-TABLE* memoizes repeated calls to KERNELIZE-NODE.
;;;
;;; Each kernel is represented by one kernel target and several kernel
;;; fragments. The target is eventually bound to some memory, before any of
;;; the fragments are run.

(defvar *use-table* nil)

(defvar *kernel-table* nil)

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
  evaluation of all GRAPH-ROOTS."
  (let ((graph-roots (ensure-list graph-roots)))
    (let ((*use-table* (inverse-table graph-roots #'inputs))
          (*kernel-table* (make-hash-table :test #'eq)))
      (map nil #'kernelize-node graph-roots)
      (hash-table-values *kernel-table*))))

(defun kernelize-node (data-structure)
  "Return a kernel whose evaluation is equivalent to the evaluation of
  DATA-STRUCTURE. Furthermore, add the kernel to the *KERNEL-TABLE*."
  (flet ((leaf? (node) (or (immediate? data-structure)
                           (> (length (gethash node *use-table*)) 1))))
    (or (gethash data-structure *kernel-table*) ; memoize calls to this function
        (and (immediate? data-structure) data-structure)
        (let* ((fragments (kernel-fragments data-structure #'leaf?))
               (result (make-instance 'kernel-target
                         :index-space (index-space data-structure)
                         :element-type (element-type data-structure)
                         :fragments fragments
                         :unevaluated-fragment-counter (length fragments))))
          ;; time for some side-effects!
          (setf (gethash data-structure *kernel-table*) result)
          (iterate
            (for fragment in fragments)
            (setf (target fragment) result)
            (iterate
              (for binding in (bindings fragment))
              (let ((leaf (kernelize-node (third binding))))
                (setf (third binding) leaf)
                (when (kernel-target? leaf)
                  (pushnew result (users leaf))))))
          result))))

