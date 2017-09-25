;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel ()
  ((recipe :type data-structure)
   (cost :type non-negative-integer)
   (number-of-dependencies :type non-negative-integer)
   (users :type list)
   (target :type immediate))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))

(defvar *kernel-table* (make-hash-table :test #'eq :weakness :key))

(defvar *use-table* nil)

(defvar *kernel-dependencies* nil)

(defvar *unblocked-kernels* nil)

(defun kernelize (graph-roots)
  "Return a list of kernels that are immediately ready for computation."
  (let ((*use-table* (inverse-table graph-roots #'inputs))
        (*unblocked-kernels* nil))
    (map nil #'kernelize-node graph-roots)
    *unblocked-kernels*))

(defun kernelize-node (data-structure)
  (or (gethash data-structure *kernel-table*)
      (let ((*kernel-dependencies* nil))
        (let ((recipe (kernelize-recipe data-structure t))
              (cost (size data-structure)))
          (let ((number-of-dependencies (length *kernel-dependencies*))
                (users (mapcar (λ user (gethash user *kernel-table*))
                               (gethash data-structure *use-table*))))
            (print (gethash data-structure *use-table*))
            (let ((kernel (make-instance 'kernel
                            :recipe recipe
                            :cost cost
                            :number-of-dependencies number-of-dependencies
                            :users users
                            :target (make-instance 'strided-array-immediate
                                      :element-type (element-type data-structure)
                                      :index-space (index-space data-structure)))))
              (setf (gethash data-structure *kernel-table*) kernel)
              (unless *kernel-dependencies*
                (push kernel *unblocked-kernels*))
              kernel))))))

(defgeneric kernelize-recipe (recipe &optional copy-unconditionally)
  (:method :around ((recipe immediate) &optional _) recipe)
  (:method :around ((recipe data-structure) &optional copy-unconditionally)
    (if (or copy-unconditionally
            (<= (length (gethash recipe *use-table*)) 1))
        (call-next-method)
        (let ((dependency (kernelize-node recipe)))
          (pushnew dependency *kernel-dependencies*)
          (target dependency))))
  (:method ((recipe t) &optional _)
    (error "Cannot kernelize the recipe ~S." recipe))
  (:method ((recipe strided-array-application) &optional _)
    (make-instance 'strided-array-application
      :operator (operator recipe)
      :element-type (element-type recipe)
      :inputs (mapcar #'kernelize-recipe (inputs recipe))
      :index-space (index-space recipe)))
  (:method ((recipe strided-array-fusion) &optional _)
    (make-instance 'strided-array-fusion
      :element-type (element-type recipe)
      :inputs (mapcar #'kernelize-recipe (inputs recipe))
      :index-space (index-space recipe)))
  (:method ((recipe strided-array-reduction) &optional _)
    (make-instance 'strided-array-reduction
      :operator (operator recipe)
      :element-type (element-type recipe)
      :inputs (mapcar #'kernelize-recipe (inputs recipe))
      :index-space (index-space recipe)))
  (:method ((recipe strided-array-reference) &optional _)
    (make-instance 'strided-array-reference
      :element-type (element-type recipe)
      :inputs (mapcar #'kernelize-recipe (inputs recipe))
      :index-space (index-space recipe)
      :transformation (transformation recipe))))

