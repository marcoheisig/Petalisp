;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

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
