;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; This is probably the most complicated part of Petalisp and deserves
;;; some explanation...
;;;
;;; The goal is to determine a sequence of kernels that compute the
;;; elements of a target immediate according to a subgraph specified by a
;;; root and a leaf function. The latter is a function returning the
;;; corresponding immediate of each leaf node and NIL for all other nodes.
;;;
;;; Most importantly, it is necessary to generate, for each kernel, a
;;; recipe that describes how it should be computed. It is crucial that
;;; this recipe is normalized, i.e. similar operations should lead to
;;; identical recipes. This makes it possible to efficiently cache and
;;; compile these recipes. In particular, the recipe must not depend on the
;;; absolute index space of any of the involved immediates. Instead, each
;;; recipe works directly on their storage.
;;;
;;; Another normalization is obtained by observing that, since the
;;; subgraphs are now much smaller than the original data flow graph, it is
;;; possible to move all references upwards until they merge into a single
;;; reference per leaf. As a result, transformations concern only the
;;; references to the sources of each kernel.
;;;
;;; The final and perhaps most controversial normalization is the
;;; elimination of all fusion nodes. Each fusion node can be eliminated by
;;; instead generating multiple kernels. The iteration space of each of
;;; these kernels is chosen such that only a single input of each fusion is
;;; utilized, effectively turning it into a reference node. The downside of
;;; this normalization step is that for subgraphs with multiple fusion
;;; nodes, the number of generated kernels grows exponentially. Time will
;;; tell whether this case occurs in practical applications.

(defun subgraph-kernels (target root leaf-function)
  "Return a list of kernels that compute TARGET, according to the data flow
   graph subtree prescribed by a ROOT node and the LEAF-FUNCTION."
  (map 'vector
       (lambda (iteration-space)
         (subgraph-kernel target root leaf-function iteration-space))
       (subgraph-iteration-spaces root leaf-function)))

(defun subgraph-kernel (target iteration-space root leaf-function)
  "Return the kernel that computes the ITERATION-SPACE of TARGET, according
   to the data flow graph prescribed by ROOT and LEAF-FUNCTION."
  (let ((sources (subgraph-sources root leaf-function)))
    (multiple-value-bind (recipe-body ranges)
        (recipe-body-builder root leaf-function iteration-space (from-storage target))
      (let* ((dimension (dimension root))
             (recipe
               (funcall
                (named-lambda build-recipe (range-id)
                  (if (= range-id 0)
                      (%store (%reference 0 (%indices (make-identity-transformation dimension)))
                              recipe-body)
                      (%for range-id (build-recipe (1+ range-id)))))
                (dimension root))))
        (make-instance 'kernel
          :target target
          :recipe recipe
          :ranges ranges
          :sources sources)))))

(defun subgraph-iteration-spaces (root leaf-function)
  "Return a partitioning of the index space of ROOT, whose elements
   describe the maximal fusion-free paths through the subgraph from ROOT to
   some leaves, as determined by the supplied LEAF-FUNCTION."
  (let ((iteration-spaces ())
        (hairy-fusions? nil))
    (labels
        ((traverse/fusion-tree? (node relevant-space transformation)
           ;; register iteration spaces and return whether a fusion node
           ;; appeared in this subtree
           (cond
             ((funcall leaf-function node) nil)
             ((fusion? node)
              (prog1 t
                (loop for input in (inputs node) do
                  (when-let ((relevant-space (intersection relevant-space (index-space input))))
                    (traverse/fusion-tree? input relevant-space transformation)
                    ;; it is important that the attempt to push happens
                    ;; after all inputs have been processed, such that
                    ;; smaller spaces take precedence over larger ones
                    ;; and we end up with maximal granularity
                    (let ((iteration-space (funcall (inverse transformation) relevant-space)))
                      (unless (some (λ s (subspace? s iteration-space)) iteration-spaces)
                        (push iteration-space iteration-spaces)))))))
             ((reference? node)
              (when-let ((relevant-space (intersection relevant-space (index-space node))))
                (traverse/fusion-tree?
                 (input node)
                 relevant-space
                 (composition (transformation node) transformation))))
             ((reduction? node)
              (traverse/fusion-tree? (input node) relevant-space transformation))
             (t
              (let ((number-of-fusing-subtrees
                      (loop for input in (inputs node)
                            count (traverse/fusion-tree? input relevant-space transformation))))
                (case number-of-fusing-subtrees
                  (0 nil)
                  (1 t)
                  (otherwise (setf hairy-fusions? t))))))))
      (traverse/fusion-tree? root (index-space root) (make-identity-transformation (dimension root))))
    ;; one case that has not yet been accounted for is when fusions appear
    ;; in more than one input of an application. In this case, iteration
    ;; spaces can overlap and it is necessary to subdivide all spaces
    ;; afterwards.
    (if hairy-fusions?
        (subdivision iteration-spaces)
        iteration-spaces)))

(defun subgraph-sources (root leaf-function)
  "Return a vector of the sources reachable from ROOT, as determined by the
  supplied LEAF-FUNCTION."
  (let ((sources (fvector)))
    (prog1 sources
      (funcall (named-lambda traverse (node)
                 (if-let ((leaf (funcall leaf-function node)))
                   (fvector-pushnew leaf sources :test #'eq)
                   (mapc #'traverse (inputs node))))
               root))))

(defun subgraph-recipe-body (node leaf-function transformation)
  "Return as values:
    1. the recipe-body
    2. all iteration ranges"
  (labels ((traverse (node relevant-space transformation)
             (if-let ((immediate (funcall leaf-function node)))
               (%reference (position immediate sources)
                           (%indices (composition (to-storage immediate) transformation)))
               (etypecase node
                 (reference
                  (traverse
                   (input node)
                   relevant-space
                   (composition (transformation node) transformation)))
                 (fusion
                  (traverse
                   (find-if (λ input (subspace relevant-space (index-space input))) (inputs node))
                   relevant-space
                   transformation))
                 (application
                  (labels ((expr-ulist (list)
                             ))
                    (%call (operator node)
                           (expr-ulist (inputs node)))))))))))

