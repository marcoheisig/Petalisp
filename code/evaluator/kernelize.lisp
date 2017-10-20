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

(defun build-dependency-graph (graph-roots)
  (let ((table (make-hash-table :test #'eq))
        (roots (ensure-sequence graph-roots)))
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
    ;; step 2 - determine the users and dependencies of each immediate
    (labels
        ((corresponding-immediate (node)
           (cond ((< (refcount node) 2) nil)
                 ((immediate? node) node)
                 (t (values (gethash node table)))))
         (register-edge (user dependency)
           (fvector-pushnew user (users dependency))
           (fvector-pushnew dependency (dependencies user)))
         (determine-dependencies (root)
           (when root
             (let ((target (corresponding-immediate root)))
               (labels ((traverse (node)
                          (if-let ((dependency (corresponding-immediate node)))
                            (register-edge target dependency)
                            (traverse-inputs node)))
                        (traverse-inputs (node)
                          (dolist (input (inputs node))
                            (traverse input))))
                 (traverse-inputs root))))))
      (maphash-values #'determine-dependencies table))
    ;; step 3 - determine the kernels of each immediate
    ))

(defvar *use-table* nil)

(defvar *kernel-table* nil)

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
  evaluation of all GRAPH-ROOTS."
  (let ((graph-roots (ensure-sequence graph-roots)))
    (let ((*use-table* (inverse-table graph-roots #'inputs))
          (*kernel-table* (make-hash-table :test #'eq)))
      (map 'vector #'kernelize-node graph-roots))))

(defun kernelize-node (data-structure)
  "Return a kernel whose evaluation is equivalent to the evaluation of
  DATA-STRUCTURE. Furthermore, add the kernel to the *KERNEL-TABLE*."
  (flet ((leaf? (node)
           (or (immediate? node)
               (and (not (eq node data-structure))
                    (/= (length (gethash node *use-table*)) 1)))))
    (or (gethash data-structure *kernel-table*) ; memoize calls to this function
        (and (immediate? data-structure) data-structure)
        (let* ((kernels (make-kernels data-structure #'leaf?))
               (result (make-instance 'intermediate-result
                         :index-space (index-space data-structure)
                         :element-type (element-type data-structure)
                         :kernels kernels)))
          ;; time for some side-effects!
          (setf (gethash data-structure *kernel-table*) result)
          (iterate
            (for kernel in kernels)
            (setf (target kernel) result)
            (let ((sources (sources kernel)))
              (map-into sources #'kernelize-node sources)
              ;(map nil (λ source (if (immediate? source) (fvector-pushnew kernel (users source)))) sources)
              ))
          result))))

(defun make-kernels (data-structure &optional (leaf? #'immediate?))
  "Return a list of kernels to compute DATA-STRUCTURE. The recipe of each
  kernel is a tree of applications and reductions, whose leaves are
  references to objects that satisfy LEAF?."
  (let (kernels)
    (map-recipes
     (lambda (recipe index-space ranges sources)
       (push (make-instance 'kernel
               :recipe recipe
               :index-space index-space
               :sources sources
               :element-type (element-type data-structure))
             kernels))
     data-structure :leaf-test leaf?)
    kernels))
