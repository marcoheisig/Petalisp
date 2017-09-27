;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel ()
  ((recipe :type data-structure)
   (cost :type non-negative-integer)
   (users :type list :initform nil :accessor users)
   (dependencies :type list)
   (input-memory :type list)
   (target :type immediate :accessor target))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))

(defun make-kernel (recipe dependencies)
  (make-instance 'kernel
    :recipe recipe
    :cost (size recipe)
    :dependencies dependencies
    :input-memory (make-list (length dependencies))
    :target (make-instance 'strided-array-immediate
              :index-space (index-space recipe)
              :element-type (element-type recipe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; conversion from data flow graphs to kernels

(defvar *kernel-table* (make-hash-table :test #'eq :weakness :key))

(defvar *use-table* nil)

(defvar *kernel-dependencies* nil)

(defvar *kernels* nil)

(defun kernelize (graph-roots)
  "Return a list of kernels that are immediately ready for computation."
  (let ((*use-table* (inverse-table graph-roots #'inputs))
        (*kernels* nil))
    (map nil #'kernelize-node graph-roots)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphviz visualization of kernel graphs

(defclass <kernel-graph> (<data-flow-graph>) ())

(defmethod graphviz-graph-plist append-plist ((purpose <kernel-graph>))
  `(:layout "dot" :splines "ortho"))

(defmethod graphviz-successors ((purpose <kernel-graph>) (kernel kernel))
  (cons (recipe kernel) (users kernel)))

(defmethod graphviz-successors ((purpose <kernel-graph>) (data-structure data-structure))
  (remove-if #'immediate? (inputs data-structure)))

(defmethod graphviz-node-plist append-plist ((purpose <kernel-graph>) (kernel kernel))
  `(:label ,(format nil "kernel ~A" (index-space (target kernel)))
    :shape "box"
    :fontsize 20))
