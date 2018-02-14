;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/build-kernel
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/blueprint
   :petalisp/core/kernel-creation/kernel
   :petalisp/core/kernel-creation/map-subtrees)
  (:export
   #:build-kernel))

(in-package :petalisp/core/kernel-creation/build-kernel)

;;; In the next step, given a particular subtree, together with the index
;;; space of a particular fragment of this subtree (i.e. one without fusion
;;; nodes) and the dimension, build a kernel. This step has several
;;; intricacies.
;;;
;;; Most importantly, all information must be split into what is necessary
;;; to generate fast code and what is not. The former part is combined into
;;; an S-expression built of unique conses, called the blueprint, the
;;; latter ends up as slot values of the kernel. In the current state,
;;; information is treated as follows:
;;;
;;; - The iteration space is not stored in the blueprint. The number of
;;;   elements of the iteration space is stored in the vector
;;;   DIMENSIONS. Start and step size are normalized to zero and one,
;;;   respectively, which implicitly causes all step sizes to be stored in
;;;   the blueprint. An approximation of the size of the iteration space,
;;;   given by a minimal and a maximal size in each dimension is also
;;;   stored in the blueprint.
;;;
;;; - Called functions are stored in the blueprint, but only if they are
;;;   known to the type inference engine. Unknown functions end up as
;;;   dynamic properties of the kernel.
;;;
;;; - The element type of each referenced immediate is converted to an
;;;   atomic type and stored in the blueprint.

(defun build-kernel (target root leaf-function relevant-space dimension)
  "Convert the data flow subtree fragment specified by ROOT, LEAF-FUNCTION
and RELEVANT-SPACE."
  (let* ((references        (make-array 10 :fill-pointer 0 :adjustable t))
         (unknown-functions (make-array 10 :fill-pointer 0 :adjustable t))
         (normalizing-transformation (index-space-normalization relevant-space))
         (bounds (make-array dimension :element-type 'array-index))
         (bounds-index (dimension relevant-space)))
    ;; initialize the initial array bounds
    (map-into bounds #'range-size (ranges relevant-space))
    ;; now traverse the tree
    (labels
        ((id (object vector)
           (or (position object vector :test #'eq)
               (vector-push-extend object vector)))
         (walk-reference (immediate transformation)
           (blueprint/reference
            (id immediate references)
            (composition (transformation immediate) transformation)))
         (walk (node relevant-space transformation)
           (when relevant-space
             (if-let ((leaf (funcall leaf-function node)))
               (walk-reference leaf transformation)
               (etypecase node
                 (application
                  (dx-flet ((walk-input (input) (walk input relevant-space transformation)))
                    (let ((operator (operator node)))
                      (blueprint/call
                       (if (symbolp operator)
                           operator
                           (id operator unknown-functions))
                       (map-ulist #'walk-input (inputs node))))))
                 (reduction
                  (let* ((input (input node))
                         (reduction-range (last-elt (ranges (index-space input))))
                         (scale (range-step reduction-range))
                         (offset (range-start reduction-range)))
                    (setf (aref bounds bounds-index)
                          (range-size reduction-range))
                    (incf bounds-index)
                    (let ((relevant-space (enlarge-index-space relevant-space (index-space input)))
                          (transformation (enlarge-transformation transformation scale offset)))
                      (blueprint/reduce
                       (binary-operator node)
                       (unary-operator node)
                       (walk input relevant-space transformation)))))
                 (fusion ;; the relevant space is already chosen to eliminate fusions
                  (let* ((input (find relevant-space (inputs node)
                                      :key #'index-space
                                      :test #'index-space-intersection-p))
                         (relevant-space (index-space-intersection relevant-space (index-space input))))
                    (walk input relevant-space transformation)))
                 (reference ;; eliminate/lift references
                  (let ((relevant-space
                          (index-space-intersection
                           (index-space (input node))
                           (funcall (transformation node) relevant-space)))
                        (transformation
                          (composition (transformation node) transformation)))
                    (walk (input node) relevant-space transformation))))))))
      (let ((blueprint-body
              (blueprint/store
               (walk-reference target normalizing-transformation)
               (walk root relevant-space normalizing-transformation))))
        (incf (refcount target))
        (make-kernel
         :bounds bounds
         :references (subseq references 0 nil)
         :unknown-functions (subseq unknown-functions 0 nil)
         :blueprint
         (blueprint/with-metadata bounds references blueprint-body))))))

(defun index-space-normalization (index-space)
  (let ((ranges (ranges index-space)))
    (affine-transformation
     :translation (map 'vector #'range-start ranges)
     :scaling (map 'vector #'range-step ranges))))
