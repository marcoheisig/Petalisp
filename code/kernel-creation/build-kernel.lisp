;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; In the next step, given a particular subtree, together with the shape
;;; of a particular fragment of this subtree (i.e. one without fusion
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
         (normalizing-transformation (shape-normalization relevant-space))
         (bounds (make-array dimension :element-type 'array-index))
         (bounds-index (dimension relevant-space)))
    ;; initialize the initial array bounds
    (map-into bounds #'set-size (ranges relevant-space))
    ;; now traverse the tree
    (labels
        ((id (object vector)
           (or (position object vector :test #'eq)
               (vector-push-extend object vector)))
         (walk-reference (immediate transformation)
           (blueprint/reference
            (id immediate references)
            (compose-transformations (transformation immediate) transformation)))
         (walk (node relevant-space transformation)
           (unless (set-emptyp relevant-space)
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
                       (ucons:map-ulist #'walk-input (inputs node))))))
                 (reduction
                  (let* ((input (input node))
                         (reduction-range (last-elt (ranges (shape input))))
                         (scale (range-step reduction-range))
                         (offset (range-start reduction-range)))
                    (setf (aref bounds bounds-index)
                          (set-size reduction-range))
                    (incf bounds-index)
                    (let ((relevant-space (enlarge-shape relevant-space (shape input)))
                          (transformation (enlarge-transformation transformation scale offset)))
                      (blueprint/reduce
                       (binary-operator node)
                       (unary-operator node)
                       (walk input relevant-space transformation)))))
                 (fusion ;; the relevant space is already chosen to eliminate fusions
                  (let* ((input (find relevant-space (inputs node)
                                      :key #'shape
                                      :test #'set-intersectionp))
                         (relevant-space (shape-intersection relevant-space (shape input))))
                    (walk input relevant-space transformation)))
                 (reference ;; eliminate/lift references
                  (let ((relevant-space
                          (set-intersection
                           (shape (input node))
                           (transform relevant-space (transformation node))))
                        (transformation
                          (compose-transformations (transformation node) transformation)))
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

(defun shape-normalization (shape)
  (let ((ranges (ranges shape)))
    (make-transformation
     :translation (map 'vector #'range-start ranges)
     :scaling (map 'vector #'range-step ranges))))
