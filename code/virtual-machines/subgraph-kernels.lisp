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

(defun subgraph-iteration-spaces (root leaf-function)
  "Return a partitioning of the index space of ROOT, whose elements
   describe the maximal fusion-free paths through the sub graph from ROOT
   to some leaves, as determined by the supplied LEAF-FUNCTION."
  (let (preimages)
    (labels ((traverse (node relevant-space transformation)
               (cond
                 ((funcall leaf-function node))
                 ((fusion? node)
                  (loop for input in (inputs node) do
                    (when-let ((relevant-space (intersection relevant-space (index-space input))))
                      (traverse input relevant-space transformation)
                      (let ((preimage (funcall (inverse transformation) relevant-space)))
                        (pushnew preimage preimages :test #'subspace?)))))
                 ((reference? node)
                  (when-let ((relevant-space (intersection relevant-space (index-space node))))
                    (traverse
                     (input node)
                     relevant-space
                     (composition (transformation node) transformation))))
                 ((or (application? node) (reduction? node))
                  (loop for input in (inputs node) do
                    (traverse input relevant-space transformation))))))
      (traverse root (index-space root) (make-identity-transformation (dimension root))))
    preimages))

(defun subgraph-kernel (target iteration-space root leaf-function)
  "Return the kernel that computes the ITERATION-SPACE of TARGET, according
   to the data flow graph prescribed by ROOT and LEAF-FUNCTION."
  (multiple-value-bind (recipe-body ranges sources)
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
        :sources sources))))

(defgeneric recipe-body-builder
    (node leaf-function relevant-space backtransformation)
  (:documentation
   "Return as values:
    1. the recipe-body
    2. all iteration ranges
    3. all sources"))
