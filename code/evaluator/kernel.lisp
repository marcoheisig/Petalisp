;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; Each Petalisp kernel consists of several input data structures, a
;;; single output data structure (WIP: support multiple values) and a
;;; recipe.

(define-class kernel (strided-array-immediate)
  ((target :type intermediate-result :accessor target)
   (recipe :type hcons)
   (bindings :type (vector immediate))))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (kernel kernel))
  `(:shape "box"
    :fillcolor "skyblue"))

(defmethod graphviz-successors
    ((purpose data-flow-graph) (kernel kernel))
  (bindings kernel))

(defmethod graphviz-edge-plist append-plist
    ((purpose data-flow-graph) (a kernel) (b immediate))
  `(:style "dashed"))

;; the iterator should be factored out as a separate utility...
(define-condition iterator-exhausted () ())

(defvar *recipe-bindings* nil)

(defvar *recipe-space* nil)

(defun make-kernels (data-structure &optional (leaf? #'immediate?))
  "Return a list of kernels to compute DATA-STRUCTURE. The recipe of each
  kernel is a tree of applications and reductions, whose leaves are
  references to objects that satisfy LEAF?."
  (let ((recipe-iterator (make-recipe-iterator data-structure leaf?))
        kernels)
    (handler-case
        (loop (let* ((*recipe-bindings* (make-array 6 :fill-pointer 0))
                     (*recipe-space* (index-space data-structure))
                     (recipe (funcall recipe-iterator)))
                (when *recipe-space*
                  (push (make-instance 'kernel
                          :recipe recipe
                          :bindings *recipe-bindings*
                          :index-space *recipe-space*
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
  (labels ((mkiter (node space transformation depth)
             ;; convert leaf nodes to an iterator with a single value
             (if (or (strided-array-constant? node)
                     (funcall leaf? node))
                 (let ((first-visit? t))
                   (λ (if first-visit?
                          (let ((index
                                  (or (position node *recipe-bindings*)
                                      (vector-push-extend node *recipe-bindings*)))
                                (transformation
                                  (composition
                                   (inverse (zero-based-transformation node))
                                   (composition
                                    transformation
                                    (zero-based-transformation data-structure)))))
                            (setf first-visit? nil)
                            (%reference (%source index) (%indices transformation)))
                          (signal 'iterator-exhausted))))
                 (etypecase node
                   ;; unconditionally eliminate fusion nodes by path
                   ;; replication. This replication process is the only
                   ;; reason why we use tree iterators. A fusion node with
                   ;; N inputs returns an iterator returning N recipes.
                   (fusion
                    (let ((input-iterators
                            (map 'vector
                                 (λ input (mkiter input (index-space input) transformation depth))
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
                                       (setf *recipe-space* space)
                                       (return (funcall input-iterator))))
                                 (iterator-exhausted ())))
                           (incf index)))))
                   ;; application nodes simply call the iterator of each input
                   (application
                    (let ((input-iterators
                            (map 'vector
                                 (λ x (mkiter x space transformation depth))
                                 (inputs node))))
                      (λ
                       (let (args)
                         (iterate (for input-iterator in-vector input-iterators downto 0)
                                  (setf args (hcons (funcall input-iterator) args)))
                         (%call (operator node) args)))))
                   ;; eliminate reference nodes entirely
                   (reference
                    (let ((new-transformation (composition (transformation node) transformation))
                          (space (intersection
                                  space
                                  (funcall (inverse transformation)
                                           (index-space node)))))
                      (mkiter (input node) space new-transformation depth)))
                   ;; reduction nodes
                   (reduction
                    (let ((input-iterator (mkiter (input node) space transformation (1+ depth))))
                      (λ (%reduce (%range depth) (operator node) (funcall input-iterator)))))))))
    (mkiter
     data-structure
     (index-space data-structure)
     (make-identity-transformation (dimension data-structure))
     (dimension data-structure))))
