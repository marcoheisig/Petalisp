;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel-fragment (strided-array-immediate)
  ((target :type kernel-target :accessor target)
   (recipe)
   (bindings :type (vector immediate))))

;; the iterator should be factored out as a separate utility...
(define-condition iterator-exhausted () ())

(defvar *kernel-fragment-bindings* nil)

(defvar *kernel-fragment-space* nil)

(defun kernel-fragments (data-structure leaf?)
  "Return a list of kernel fragments to compute DATA-STRUCTURE. The recipe
  of each fragment is a tree of applications and reductions, whose leaves
  are references to objects that satisfy LEAF?."
  (let ((recipe-iterator (make-recipe-iterator data-structure leaf?))
        fragments)
    (handler-case
        (loop (let* ((*kernel-fragment-bindings* (make-array 6 :fill-pointer 0))
                     (*kernel-fragment-space* (index-space data-structure))
                     (recipe (funcall recipe-iterator)))
                (push (make-instance 'kernel-fragment
                        :recipe recipe
                        :bindings *kernel-fragment-bindings*
                        :index-space *kernel-fragment-space*
                        :element-type (element-type data-structure))
                      fragments)))
      (iterator-exhausted ()))
    fragments))

(defun make-recipe-iterator (data-structure leaf?)
  "A recipe iterator is a THUNK that yields upon each iteration either a
  new recipe, or NIL, when there are no more recipes."
  (labels ((mkiter (node transformation)
             ;; TRANSFORMATION is a mapping from the iteration space to the
             ;; current index space

             ;; leaf nodes are converted to an iterator over a single value
             (if (or (strided-array-constant? node)
                     (funcall leaf? node))
                 (let ((first-visit? t))
                   (λ (if first-visit?
                          (let ((index
                                  (or (position node *kernel-fragment-bindings*)
                                      (vector-push-extend node *kernel-fragment-bindings*))))
                            (setf first-visit? nil)
                            `(%reference ,transformation ,index))
                          (signal 'iterator-exhausted))))
                 (etypecase node
                   ;; fusion nodes are unconditionally eliminated by path
                   ;; replication. This replication process is the only
                   ;; reason why we use tree iterators. A fusion node with
                   ;; N inputs returns an iterator returning N recipes.
                   (fusion
                    (let ((input-iterators
                            (map 'vector
                                 (λ input (mkiter input transformation))
                                 (inputs node)))
                          (spaces
                            (map 'vector #'index-space (inputs node)))
                          (index 0))
                      (λ (loop
                           (if (= index (length input-iterators))
                               (signal 'iterator-exhausted)
                               (handler-case
                                   (let ((input-index-space (aref spaces index))
                                         (input-iterator (aref input-iterators index)))
                                     (setf *kernel-fragment-space*
                                           (funcall (inverse transformation) input-index-space))
                                     (return (funcall input-iterator)))
                                 (iterator-exhausted ())))
                           (incf index)))))
                   ;; application nodes simply call the iterator of each input
                   (application
                    (let ((input-iterators
                            (map 'vector
                                 (λ x (mkiter x transformation))
                                 (inputs node))))
                      (λ `(%application ,(operator node)
                                        ,@(map 'list #'funcall input-iterators)))))
                   ;; reference nodes are eliminated entirely
                   (reference
                    (let ((new-transformation (composition (transformation node) transformation)))
                      (mkiter (input node) new-transformation)))
                   ;; reduction nodes
                   (reduction
                    (let ((input-iterator (mkiter (input node) transformation)))
                      (λ `(%reduction ,(operator node) ,(funcall input-iterator)))))))))
    (mkiter data-structure (make-identity-transformation (dimension data-structure)))))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (kernel-fragment kernel-fragment))
  `(:shape "box"
    :fillcolor "skyblue"))

(defmethod graphviz-successors
    ((purpose data-flow-graph) (kernel-fragment kernel-fragment))
  (list (recipe kernel-fragment)))

(defmethod graphviz-successors ((purpose data-flow-graph) (list cons))
  (ecase (car list)
    (%application (cddr list))
    (%reduction (cddr list))
    (%reference nil)))

(defmethod graphviz-node-plist append-plist ((purpose data-flow-graph) (list cons))
  (ecase (car list)
    (%application `(:label ,(format nil "(α ~A)" (second list)) :fillcolor "indianred1"))
    (%reduction `(:label ,(format nil "(β ~A)" (second list)) :fillcolor "indianred3"))
    (%reference `(:label ,(format nil "~A" (second list)) :fillcolor "gray"))))
