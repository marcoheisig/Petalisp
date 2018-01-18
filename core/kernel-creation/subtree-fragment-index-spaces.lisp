;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/subtree-fragment-index-spaces
  (:use :closer-common-lisp :alexandria :iterate)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/kernel)
  (:export
   #:subtree-fragment-index-spaces))

(in-package :petalisp/core/kernel-creation/subtree-fragment-index-spaces)

(defun subtree-fragment-index-spaces (root leaf-function)
  "Returns a partitioning of the index space of ROOT, whose elements
describe the maximal fusion-free paths through the subgraph from ROOT to
some leaves, as determined by the supplied LEAF-FUNCTION."
  (labels
      ;; walk the tree starting from ROOT, up to the leaves as determined
      ;; by LEAF-FUNCTION. RELEVANT-SPACE is a subspace of the index space
      ;; of ROOT, that shrinks when selecting a particular input of a
      ;; fusion node and potentially on reference nodes. TRANSFORMATION is
      ;; a mapping between the iteration space of the current node and the
      ;; iteration space of the root node.
      ((fragment-spaces (node relevant-space transformation)
         ;; NODE is the root of the current subtree. FRAGMENT-SPACES
         ;; returns a list of all iteration spaces in the current subtree,
         ;; or NIL, if the subtree contains no fusions.
         (unless (funcall leaf-function node)
           (typecase node
             (fusion
              (iterate
                (for input in (inputs node))
                (when-let ((subspace (index-space-intersection relevant-space (index-space input))))
                  (nconcing
                   (or (fragment-spaces input subspace transformation)
                       (list (funcall (inverse transformation) subspace)))))))
             (reference
              (when-let ((subspace (index-space-intersection relevant-space (index-space node))))
                (fragment-spaces (input node) subspace
                                 (composition (transformation node) transformation))))
             (reduction
              (let ((input (input node)))
                (let ((relevant-space (enlarge-index-space relevant-space (index-space input)))
                      (transformation (enlarge-transformation transformation 1 0)))
                  (fragment-spaces (input node) relevant-space transformation))))
             (application
              (let* ((number-of-fusing-subtrees 0)
                     (index-spaces
                       (iterate
                         (for input in (inputs node))
                         (when-let ((spaces (fragment-spaces input relevant-space transformation)))
                           (incf number-of-fusing-subtrees)
                           (nconcing spaces)))))
                (if (> number-of-fusing-subtrees 1)
                    (subdivision index-spaces)
                    index-spaces)))))))
    (or
     (fragment-spaces root (index-space root) (identity-transformation (dimension root)))
     (list (index-space root)))))
