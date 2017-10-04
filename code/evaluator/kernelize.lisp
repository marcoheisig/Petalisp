;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; Kernelization is the process of breaking a data flow specification into
;;; a set of executable parts. The data flow specification is given by a
;;; set of graph roots (usually the arguments passed to SCHEDULE) and forms
;;; a DAG (directed acyclic graph).
;;;
;;; The following algorithm cuts such a DAG into as large as possible
;;; kernels. All inputs of a kernel, up to the beginning of the next
;;; kernels form a tree, i.e. they have exactly one successor. Later, this
;;; allows to build a corresponding s-expression to actually evaluate each
;;; kernel.
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
;;; *KERNEL-TABLE* memoizes repeated calls to KERNELIZE-NODE. While
;;; copying, the system also removes all fusion nodes and lifts all
;;; reference nodes to the leaves of the kernel recipe.
;;;
;;; Each kernel is represented by one kernel target and several kernel
;;; fragments. The target is eventually bound to some memory, before any of
;;; the fragments are run.

(defvar *use-table* nil)

(defvar *kernel-table* nil)

(defvar *kernel-bindings* nil)

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
  (or (gethash data-structure *kernel-table*) ; memoize calls to this function
      (and (immediate? data-structure) data-structure)
      (let* ((fragments (kernel-fragments data-structure))
             (result (make-instance 'kernel-target
                       :index-space (index-space data-structure)
                       :element-type (element-type data-structure)
                       :fragments fragments
                       :unevaluated-fragment-counter (length fragments))))
        (setf (gethash data-structure *kernel-table*) result)
        (iterate (for fragment in fragments)
                 (setf (target fragment) result)
                 (iterate (for binding in (bindings fragment))
                          (setf (third binding)
                                (kernelize-node (third binding)))))
        result)))

(define-condition iterator-exhausted () ())

(defun kernel-fragments (data-structure)
  (let ((recipe-iterator (make-recipe-iterator data-structure))
        fragments)
    (handler-case
        (loop (let* ((*kernel-bindings* nil)
                     (recipe (funcall recipe-iterator)))
                (push (make-instance 'kernel-fragment
                        :recipe recipe
                        :bindings *kernel-bindings*
                        :index-space (index-space data-structure)) ; TODO wrong
                      fragments)))
      (iterator-exhausted ()))
    fragments))

(defun make-recipe-iterator (data-structure)
  "A recipe iterator is a THUNK that yields upon each iteration either a
  new recipe, or NIL, when there are no more recipes."
  (labels ((mkiter (node transformation)
             (if (or (immediate? node)
                     (and (not (eq node data-structure))
                          (> (length (gethash node *use-table*)) 1)))
                 (let ((first-visit? t))
                   (lambda ()
                     (if first-visit?
                         (prog1 (let ((form `(reference ,transformation ,node)))
                                  (push form *kernel-bindings*)
                                  form)
                           (setf first-visit? nil))
                         (signal 'iterator-exhausted))))
                 (etypecase node
                   (fusion
                    (let* ((input-iterators (map 'vector (λ x (mkiter x transformation))
                                                 (inputs node)))
                           (index 0))
                      (lambda ()
                        (loop
                          (if (= index (length input-iterators))
                              (signal 'iterator-exhausted)
                              (handler-case (return (funcall (aref input-iterators index)))
                                (iterator-exhausted ())))
                          (incf index)))))
                   (application
                    (let ((input-iterators (map 'vector (λ x (mkiter x transformation))
                                                (inputs node))))
                      (lambda ()
                          (let ((operands (map 'list #'funcall input-iterators)))
                            `(application ,(operator node) ,@operands)))))
                   (reference
                    (mkiter (input node) (composition (transformation node) transformation)))
                   (reduction
                    (let ((input-iterator (mkiter (input node) transformation)))
                      (lambda ()
                        `(reduction ,(operator node) ,(funcall input-iterator)))))))))
    (mkiter data-structure (make-identity-transformation (dimension data-structure)))))
