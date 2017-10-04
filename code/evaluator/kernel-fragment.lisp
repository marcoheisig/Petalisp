;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel-fragment (strided-array-immediate)
  ((target :type kernel-target :accessor target)
   (recipe)
   (bindings)))

;; the iterator should be factored out as a separate utility...
(define-condition iterator-exhausted () ())

(defvar *kernel-fragment-bindings* nil)

(defun kernel-fragments (data-structure leaf?)
  "Return a list of kernel fragments to compute DATA-STRUCTURE. The recipe
  of each fragment is a tree of applications and reductions, whose leaves
  are references to objects that satisfy LEAF?."
  (let ((recipe-iterator (make-recipe-iterator data-structure leaf?))
        fragments)
    (handler-case
        (loop (let* ((*kernel-fragment-bindings* nil)
                     (recipe (funcall recipe-iterator)))
                (push (make-instance 'kernel-fragment
                        :recipe recipe
                        :bindings *kernel-fragment-bindings*
                        :index-space (index-space data-structure) ; this space is too large
                        :element-type (element-type data-structure))
                      fragments)))
      (iterator-exhausted ()))
    fragments))

(defun make-recipe-iterator (data-structure leaf?)
  "A recipe iterator is a THUNK that yields upon each iteration either a
  new recipe, or NIL, when there are no more recipes."
  (labels ((mkiter (node transformation)
             (if (or (immediate? node)
                     (and (not (eq node data-structure))
                          (funcall leaf? node)))
                 (let ((first-visit? t))
                   (λ (if first-visit?
                          (prog1 (let ((form `(reference ,transformation ,node)))
                                   (push form *kernel-fragment-bindings*)
                                   form)
                            (setf first-visit? nil))
                          (signal 'iterator-exhausted))))
                 (etypecase node
                   (fusion
                    (let* ((input-iterators (map 'vector (λ x (mkiter x transformation))
                                                 (inputs node)))
                           (index 0))
                      (λ (loop
                           (if (= index (length input-iterators))
                               (signal 'iterator-exhausted)
                               (handler-case (return (funcall (aref input-iterators index)))
                                 (iterator-exhausted ())))
                           (incf index)))))
                   (application
                    (let ((input-iterators (map 'vector (λ x (mkiter x transformation))
                                                (inputs node))))
                      (λ (let ((operands (map 'list #'funcall input-iterators)))
                           `(application ,(operator node) ,@operands)))))
                   (reference
                    (mkiter (input node) (composition (transformation node) transformation)))
                   (reduction
                    (let ((input-iterator (mkiter (input node) transformation)))
                      (λ `(reduction ,(operator node) ,(funcall input-iterator)))))))))
    (mkiter data-structure (make-identity-transformation (dimension data-structure)))))

(defmethod graphviz-node-plist append-plist
    ((purpose <data-flow-graph>) (kernel-fragment kernel-fragment))
  `(:shape "box"
    :fillcolor "skyblue"))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (kernel-fragment kernel-fragment))
  (list (recipe kernel-fragment)))
