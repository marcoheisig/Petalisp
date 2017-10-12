;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel (strided-array-immediate)
  ((target :type intermediate-result :accessor target)
   (recipe)
   (bindings :type (vector immediate))))

;; the iterator should be factored out as a separate utility...
(define-condition iterator-exhausted () ())

(defvar *kernel-bindings* nil)

(defvar *kernel-space* nil)

(defun generate-kernels (data-structure leaf?)
  "Return a list of kernels to compute DATA-STRUCTURE. The recipe of each
  kernel is a tree of applications and reductions, whose leaves are
  references to objects that satisfy LEAF?."
  (let ((recipe-iterator (make-recipe-iterator data-structure leaf?))
        kernels)
    (handler-case
        (loop (let* ((*kernel-bindings* (make-array 6 :fill-pointer 0))
                     (*kernel-space* (index-space data-structure))
                     (recipe (funcall recipe-iterator)))
                (when *kernel-space*
                  (push (make-instance 'kernel
                          :recipe recipe
                          :bindings *kernel-bindings*
                          :index-space *kernel-space*
                          :element-type (element-type data-structure))
                        kernels))))
      (iterator-exhausted ()))
    kernels))

(defun zero-based-transformation (data-structure)
  (let* ((ranges (ranges (index-space data-structure)))
         (dimension (length ranges)))
    (make-affine-transformation
     (make-array dimension :initial-element nil)
     (scaled-permutation-matrix
      dimension dimension
      (apply #'vector (iota dimension))
      (map 'vector #'range-step ranges))
     (map 'vector #'range-start ranges))))

(defun make-recipe-iterator (data-structure leaf?)
  "A recipe iterator is a THUNK that yields upon each iteration either a
  new recipe, or NIL, when there are no more recipes."
  (labels ((mkiter (node space transformation)
             ;; TRANSFORMATION is a mapping from the iteration space to the
             ;; current index space

             ;; convert leaf nodes to an iterator with a single value
             (if (or (strided-array-constant? node)
                     (funcall leaf? node))
                 (let ((first-visit? t))
                   (λ (if first-visit?
                          (let ((index
                                  (or (position node *kernel-bindings*)
                                      (vector-push-extend node *kernel-bindings*))))
                            (setf first-visit? nil)
                            `(%reference
                              ,(composition
                                (inverse (zero-based-transformation node))
                                (composition
                                 transformation
                                 (zero-based-transformation data-structure)))
                              ,index))
                          (signal 'iterator-exhausted))))
                 (etypecase node
                   ;; unconditionally eliminate fusion nodes by path
                   ;; replication. This replication process is the only
                   ;; reason why we use tree iterators. A fusion node with
                   ;; N inputs returns an iterator returning N recipes.
                   (fusion
                    (let ((input-iterators
                            (map 'vector
                                 (λ input (mkiter input (index-space input) transformation))
                                 (inputs node)))
                          (spaces (map 'vector #'index-space (inputs node)))
                          (index 0))
                      (λ (loop
                           (if (= index (length input-iterators))
                               (signal 'iterator-exhausted)
                               (handler-case
                                   (let ((input-iterator (aref input-iterators index))
                                         (space (intersection
                                                 space
                                                 (funcall (inverse transformation)
                                                          (aref spaces index)))))
                                     (when space
                                       (setf *kernel-space* space)
                                       (return (funcall input-iterator))))
                                 (iterator-exhausted ())))
                           (incf index)))))
                   ;; application nodes simply call the iterator of each input
                   (application
                    (let ((input-iterators
                            (map 'vector
                                 (λ x (mkiter x space transformation))
                                 (inputs node))))
                      (λ `(%application ,(operator node)
                                        ,@(map 'list #'funcall input-iterators)))))
                   ;; eliminate reference nodes entirely
                   (reference
                    (let ((new-transformation (composition (transformation node) transformation))
                          (space (intersection
                                  space
                                  (funcall (inverse transformation)
                                           (index-space node)))))
                      (mkiter (input node) space new-transformation)))
                   ;; reduction nodes
                   (reduction
                    (let ((input-iterator (mkiter (input node) space transformation)))
                      (λ `(%reduction ,(operator node) ,(funcall input-iterator)))))))))
    (mkiter
     data-structure
     (index-space data-structure)
     (make-identity-transformation (dimension data-structure)))))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (kernel kernel))
  `(:shape "box"
    :fillcolor "skyblue"))

(defmethod graphviz-successors
    ((purpose data-flow-graph) (kernel kernel))
  (list* (recipe kernel) (map 'list #'identity (bindings kernel))))

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
