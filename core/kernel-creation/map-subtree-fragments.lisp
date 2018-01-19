;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/map-subtree-fragments
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/kernel)
  (:export
   #:map-subtree-fragments))

(in-package :petalisp/core/kernel-creation/map-subtree-fragments)

(defun map-subtree-fragments (fragment-fn root leaf-function)
  "Invoke FRAGMENT-FN on each fusion-free subtree fragment of the tree
starting from ROOT with leafs denoted by LEAF-FUNCTION. For each subtree
fragment, FRAGMENT-FN receives the following arguments:

1. the index space of the fragment, which is a subspace of the index space
   of root
2. the dimension of the iteration space of the fragment"
  (let ((results (make-array 8 :fill-pointer 0)))
    (labels
        ((walk-potential-outer-node (node index-space transformation)
           (multiple-value-bind (fusion-free? n-reductions)
               (walk node index-space transformation)
             (when fusion-free?
               (vector-push-extend
                (funcall fragment-fn
                         (funcall (inverse transformation) index-space)
                         (+ (dimension root) n-reductions))
                results))
             (values fusion-free? n-reductions)))
         (walk (node index-space transformation)
           ;; Case 1: Leaves
           (if (funcall leaf-function node)
               (values 0 0)
               (etypecase node
                 ;; Case 2: Fusions
                 (fusion
                  (let ((total-reductions 0))
                    (dolist (input (inputs node))
                      (when-let ((subspace (index-space-intersection index-space (index-space input))))
                        (multiple-value-bind (fusion-free? n-reductions)
                            (walk-potential-outer-node input subspace transformation)
                          (declare (ignore fusion-free?))
                          (setf total-reductions (max total-reductions n-reductions)))))
                    (values nil total-reductions)))
                 ;; Case 3: References
                 (reference
                  (when-let ((subspace (index-space-intersection index-space (index-space node))))
                    (let ((transformation (composition (transformation node) transformation)))
                      (multiple-value-bind (fusion-free? n-reductions)
                          (walk (input node) subspace transformation)
                        (values fusion-free? n-reductions)))))
                 ;; Case 3: Reductions
                 (reduction
                  (let* ((input (input node))
                         (index-space (enlarge-index-space index-space (index-space input)))
                         (transformation (enlarge-transformation transformation 1 0)))
                    (multiple-value-bind (fusion-free? n-reductions)
                        (walk input index-space transformation)
                      (values fusion-free? (1+ n-reductions)))))
                 ;; Case 4: Applications
                 (application
                  (let ((every-fusion-free? t)
                        (total-reductions 0))
                    (dolist (input (inputs node))
                      (multiple-value-bind (fusion-free? n-reductions)
                          (walk input index-space transformation)
                        (when (not fusion-free?)
                          (setf every-fusion-free? nil))
                        (setf total-reductions (max n-reductions total-reductions))))
                    (values every-fusion-free? total-reductions)))))))
      (walk-potential-outer-node root (index-space root) (identity-transformation (dimension root))))
    (subseq results 0 nil)))
