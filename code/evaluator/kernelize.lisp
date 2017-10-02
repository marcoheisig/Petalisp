;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *kernel-table* (make-hash-table :test #'eq :weakness :key))

(defvar *use-table* nil)

(defvar *kernel-bindings* nil)

(defvar *kernels* nil)

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
  evaluation of all GRAPH-ROOTS."
  (let ((graph-roots (ensure-list graph-roots)))
    (let ((*use-table* (inverse-table graph-roots #'inputs))
          (*kernels* nil))
      (map nil #'kernelize-node graph-roots)
      *kernels*)))

(defun kernelize-node (data-structure)
  "Return a kernel whose evaluation is equivalent to the evaluation of
  DATA-STRUCTURE. Furthermore, add the kernel to *KERNELS* and the
  *KERNEL-TABLE*."
  (cond ((immediate? data-structure) data-structure)
        ((gethash data-structure *kernel-table*))
        (t
         (multiple-value-bind (recipes bindings)
             (generate-recipes data-structure)
           (let ((kernel (make-kernel (index-space data-structure) recipes bindings)))
             (push kernel *kernels*)
             (setf (gethash data-structure *kernel-table*) kernel)
             (iterate (for bindings in-vector (bindings kernel))
                      (iterate (for binding in bindings)
                               (setf (third binding)
                                     (kernelize-node (third binding)))))
             kernel)))))

(define-condition iterator-exhausted () ())

(defun generate-recipes (data-structure)
  "Return a vector of recipes and a vector of bindings."
  (let ((recipe-iterator (make-recipe-iterator data-structure))
        (*kernel-bindings* nil)
        recipes bindings)
    (handler-case
        (iterate (setf *kernel-bindings* nil)
                 (for recipe = (funcall recipe-iterator))
                 (push recipe recipes)
                 (push *kernel-bindings* bindings))
      (iterator-exhausted ()))
    (values (apply #'vector recipes)
            (apply #'vector bindings))))

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
