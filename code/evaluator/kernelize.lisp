;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *kernel-table* (make-hash-table :test #'eq :weakness :key))

(defvar *use-table* nil)

(defvar *kernel-dependencies* nil)

(defvar *kernels* nil)

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
  evaluation of all GRAPH-ROOTS."
  (setf graph-roots (ensure-list graph-roots))
  (let ((*use-table* (inverse-table graph-roots #'inputs))
        (*kernels* nil))
    (mapc #'kernelize-node graph-roots)
    (dolist (kernel *kernels*)
      (dolist (dependency (dependencies kernel))
        (pushnew kernel (users dependency))))
    *kernels*))

(defun kernelize-node (data-structure)
  "Return a kernel whose evaluation is equivalent to the evaluation of
  DATA-STRUCTURE."
  (let* ((*kernel-dependencies* nil)
         (recipe (kernelize-and-copy data-structure))
         (kernel (make-kernel recipe *kernel-dependencies*)))
    (push kernel *kernels*)
    (setf (gethash data-structure *kernel-table*) kernel)))

(defun kernelize-input (input-node)
  "Return a copy of INPUT-NODE that is either an immediate value, or a node
  with exactly one user. If a node has more than one user, it is replaced
  by the target of another kernel."
  (cond ((immediate? input-node) input-node)
        ((= 1 (length (gethash input-node *use-table*)))
         (kernelize-and-copy input-node))
        (t (let ((kernel (or (gethash input-node *kernel-table*)
                             (kernelize-node input-node))))
             (pushnew kernel *kernel-dependencies*)
             (target kernel)))))

(defgeneric kernelize-and-copy (node)
  (:documentation
   "Return a copy of NODE with kernelized inputs.")
  (:method ((node immediate)) node)
  (:method ((node strided-array-application))
    (make-instance 'strided-array-application
      :operator (operator node)
      :element-type (element-type node)
      :inputs (mapcar #'kernelize-input (inputs node))
      :index-space (index-space node)))
  (:method ((node strided-array-fusion))
    (make-instance 'strided-array-fusion
      :element-type (element-type node)
      :inputs (mapcar #'kernelize-input (inputs node))
      :index-space (index-space node)))
  (:method ((node strided-array-reduction))
    (make-instance 'strided-array-reduction
      :operator (operator node)
      :element-type (element-type node)
      :inputs (list (kernelize-input (input node)))
      :index-space (index-space node)))
  (:method ((node strided-array-reference))
    (make-instance 'strided-array-reference
      :element-type (element-type node)
      :inputs (mapcar #'kernelize-input (inputs node))
      :index-space (index-space node)
      :transformation (transformation node))))
