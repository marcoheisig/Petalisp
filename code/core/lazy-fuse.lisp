;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-fuse (array &rest more-arrays)
  ;; No need to fuse when only a single array is supplied.
  (when (null more-arrays)
    (return-from lazy-fuse (lazy-array array)))
  (let ((arrays (list* array more-arrays)))
    (multiple-value-bind (lazy-arrays result-shape)
        (broadcast-for-fusion arrays)
      ;; Check that all the supplied arrays are pairwise disjoint.
      (unless (= (shape-size result-shape)
                 (reduce #'+ lazy-arrays :key #'lazy-array-size))
        ;; Find out which of the supplied arrays overlap.
        (loop for lazy-array-1 in lazy-arrays for i from 0 do
          (loop for lazy-array-2 in lazy-arrays for j below i do
            (when (shape-intersectionp
                   (lazy-array-shape lazy-array-1)
                   (lazy-array-shape lazy-array-2))
              (error "~@<Can only fuse disjoint shapes, ~
                       but the arrays ~S and the shape ~S have the ~
                       common subshape ~S.~:@>"
                     (nth i arrays)
                     (nth j arrays)
                     (shape-intersection
                      (lazy-array-shape lazy-array-1)
                      (lazy-array-shape lazy-array-2))))))
        (error "~@<Cannot cover the predicted fusion shape ~S with the supplied data.~:@>"
               result-shape))
      (let ((ntype (reduce #'typo:ntype-union lazy-arrays :key #'lazy-array-ntype)))
        ;; Optimization: If the content of the fusion is predicted to be a
        ;; constant, we replace the entire fusion by a reference to that constant.
        (when (typo:eql-ntype-p ntype)
          (return-from lazy-fuse
            (lazy-ref
             (lazy-array-from-scalar (typo:eql-ntype-object ntype))
             result-shape
             (make-transformation
              :input-rank (shape-rank result-shape)
              :output-rank 0))))
        ;; Optimization: If the fusion is equivalent to a broadcasting reference
        ;; (i.e., when all inputs are lazy reshapes of the same lazy array, and all
        ;; transformations of those reshapes are similar), we replace the entire
        ;; fusion by such a broadcasting reference.
        (when (delayed-reshape-p (lazy-array-delayed-action (first lazy-arrays)))
          (let* ((delayed-reshape (lazy-array-delayed-action (first lazy-arrays)))
                 (first-input (delayed-reshape-input delayed-reshape))
                 (first-transformation (delayed-reshape-transformation delayed-reshape))
                 (first-output-mask (transformation-output-mask first-transformation))
                 (first-scalings (transformation-scalings first-transformation))
                 (first-offsets (transformation-offsets first-transformation)))
            (when (loop for lazy-array in (rest lazy-arrays)
                        for action = (lazy-array-delayed-action lazy-array)
                        always
                        (and (delayed-reshape-p action)
                             (eq (delayed-reshape-input action) first-input)
                             (let ((transformation (delayed-reshape-transformation action)))
                               (and (equalp first-output-mask (transformation-output-mask transformation))
                                    (equalp first-scalings (transformation-scalings transformation))
                                    (equalp first-offsets (transformation-offsets transformation))))))
              (return-from lazy-fuse
                (lazy-ref
                 first-input
                 result-shape
                 (make-transformation
                  :input-rank (transformation-input-rank first-transformation)
                  :output-mask first-output-mask
                  :scalings first-scalings
                  :offsets first-offsets))))))
        ;; Default: Create a lazy array that performs the specified fusion.
        (make-lazy-array
         :delayed-action (make-delayed-fuse :inputs lazy-arrays)
         :shape result-shape
         :ntype ntype
         :depth (1+ (maxdepth lazy-arrays)))))))

(defun broadcast-for-fusion (arrays)
  "Returns a list of lazy arrays, one for each supplied array, that are all broadcast
to have the same rank, in a way that is compatible with array fusion.  As a
second value, returns the shape of the resulting fusion.  Doesn't check whether
the fusion would be valid."
  (let* ((arrays (list* arrays))
         (lazy-arrays (mapcar #'lazy-array arrays))
         (shapes (mapcar #'lazy-array-shape lazy-arrays))
         (result-shape (superimpose-shapes shapes))
         (result-rank (shape-rank result-shape)))
    (values
     (loop for lazy-array in lazy-arrays
           collect
           (let ((rank (lazy-array-rank lazy-array)))
             (if (= rank result-rank)
                 lazy-array
                 (lazy-reshape
                  lazy-array
                  (make-shape
                   (append
                    (lazy-array-ranges lazy-array)
                    (subseq (shape-ranges result-shape) rank)))))))
     result-shape)))
