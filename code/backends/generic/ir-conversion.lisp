;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are strided arrays, into an analogous graph, whose nodes are buffers
;;; and kernels.  Each buffer is read from --- or written to --- by one or
;;; more kernels.  Each kernel has zero or more input buffers and one or
;;; more output buffer.
;;;
;;; The default IR conversion algorithm proceeds in the following steps:
;;;
;;; 1. A hash table is created that maps certain strided arrays to buffers
;;;    of the same size and element type.  This table is constructed such
;;;    that any subgraph without these nodes is a tree and contains no
;;;    reduction nodes.
;;;
;;; 2. The root of each resulting tree from step 1 is split such that the
;;;    resulting trees are free of fusion nodes.
;;;
;;; 3. Each fusion-free tree from step 2 is turned into a kernel.
;;;
;;; 4. All buffers are updated to contain a list of kernels that read to
;;;    them or write from them.


(defun ir-from-strided-arrays (strided-arrays backend)
  (let ((buffer-table (make-buffer-table strided-arrays backend)))
    (loop for root being each hash-key of buffer-table
            using (hash-value buffer) do
              (values) ;; TODO
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Subtree Fragments to Kernels
;;;
;;; Given a particular subtree, together with the shape of a particular
;;; fragment of this subtree (i.e. one without fusion nodes) and the
;;; dimension, build a kernel.  This step has several intricacies.
;;;
;;; Most importantly, all information must be split into what is necessary
;;; to generate fast code and what is not.  The former part is combined
;;; into an S-expression built of unique conses, called the blueprint, the
;;; latter ends up as slot values of the kernel.  In the current state,
;;; information is treated as follows:
;;;
;;; - The iteration space is not stored in the blueprint.  The number of
;;;   elements of the iteration space is stored in the vector DIMENSIONS.
;;;   Start and step size are normalized to zero and one, respectively,
;;;   which implicitly causes all step sizes to be stored in the blueprint.
;;;   An approximation of the size of the iteration space, given by a
;;;   minimal and a maximal size in each dimension is also stored in the
;;;   blueprint.
;;;
;;; - Called functions are stored in the blueprint, but only if they are
;;;   known to the type inference engine.  Unknown functions end up as
;;;   dynamic properties of the kernel.
;;;
;;; - The element type of each referenced immediate is converted to an
;;;   atomic type and stored in the blueprint.

(defun map-subtree-fragments (fragment-fn root leaf-function)
  "Invoke FRAGMENT-FN on each fusion-free subtree fragment of the tree
starting from ROOT with leafs denoted by LEAF-FUNCTION. For each subtree
fragment, FRAGMENT-FN receives the following arguments:

1. the shape of the fragment, which is a subspace of the shape of root
   2. the dimension of the iteration space of the fragment"
  (let ((results (make-array 8 :fill-pointer 0 :adjustable t)))
    (labels
        ((walk-potential-outer-node (node shape transformation)
           (multiple-value-bind (fusion-free? n-reductions)
               (walk node shape transformation)
             (when fusion-free?
               (vector-push-extend
                (funcall fragment-fn
                         (transform shape (invert-transformation transformation))
                         (+ (dimension root) n-reductions))
                results))
             (values fusion-free? n-reductions)))
         (walk (node shape transformation)
           ;; Case 1: Leaves
           (if (funcall leaf-function node)
               (values t 0)
               (etypecase node
                 ;; Case 2: Fusions
                 (fusion
                  (let ((total-reductions 0))
                    (declare (non-negative-fixnum total-reductions))
                    (dolist (input (inputs node))
                      (let ((subspace (set-intersection shape (shape input))))
                        (unless (set-emptyp subspace)
                          (multiple-value-bind (fusion-free? n-reductions)
                              (walk-potential-outer-node input subspace transformation)
                            (declare (ignore fusion-free?))
                            (setf total-reductions (max total-reductions n-reductions))))))
                    (values nil total-reductions)))
                 ;; Case 3: References
                 (reference
                  (let ((transformation (compose-transformations (transformation node) transformation))
                        (subspace (set-intersection shape (shape node))))
                    (multiple-value-bind (fusion-free? n-reductions)
                        (walk (input node) subspace transformation)
                      (values fusion-free? n-reductions))))
                 ;; Case 4: Reductions
                 (reduction
                  (let* ((input (input node))
                         (shape (enlarge-shape shape (shape input)))
                         (transformation (enlarge-transformation transformation 1 0)))
                    (multiple-value-bind (fusion-free? n-reductions)
                        (walk input shape transformation)
                      (values fusion-free? (1+ n-reductions)))))
                 ;; Case 5: Applications
                 (application
                  (let ((every-fusion-free? t)
                        (total-reductions 0))
                    (declare (non-negative-fixnum total-reductions))
                    (dolist (input (inputs node))
                      (multiple-value-bind (fusion-free? n-reductions)
                          (walk input shape transformation)
                        (when (not fusion-free?)
                          (setf every-fusion-free? nil))
                        (setf total-reductions (max n-reductions total-reductions))))
                    (values every-fusion-free? total-reductions)))))))
      (declare (ftype (function (t t t) (values boolean non-negative-fixnum))
                      walk walk-potential-outer-node))
      (walk-potential-outer-node
       root
       (shape root)
       (make-identity-transformation (dimension root))))
    (subseq results 0 nil)))
