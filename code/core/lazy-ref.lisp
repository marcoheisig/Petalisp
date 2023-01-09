;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-ref (input shape transformation)
  (declare (lazy-array input)
           (shape shape)
           (transformation transformation))
  (let ((relevant-shape (transform-shape shape transformation))
        (input-shape (lazy-array-shape input)))
    (unless (and (= (shape-rank relevant-shape)
                    (shape-rank input-shape))
                 (subshapep relevant-shape input-shape))
      (error "~@<Invalid reference to ~S with shape ~S and transformation ~S.~:@>"
             input shape transformation))
    (labels
        ((ref (lazy-array relevant-shape transformation)
           (declare (lazy-array lazy-array)
                    (shape relevant-shape)
                    (transformation transformation))
           ;; Optimization: Drop references with no effect.
           (when (and (identity-transformation-p transformation)
                      (shape= (lazy-array-shape lazy-array) shape))
             (return-from ref lazy-array))
           ;; Optimization: Compose consecutive lazy reshapes.
           (when (delayed-reshape-p (lazy-array-delayed-action lazy-array))
             (let ((delayed-reshape (lazy-array-delayed-action lazy-array)))
               (return-from ref
                 (ref
                  (delayed-reshape-input delayed-reshape)
                  (transform-shape
                   relevant-shape
                   (delayed-reshape-transformation delayed-reshape))
                  (compose-transformations
                   (delayed-reshape-transformation delayed-reshape)
                   transformation)))))
           ;; Optimization: Skip references to lazy fuse operations in case
           ;; they fall entirely within a single input of that fusion.
           (when (delayed-fuse-p (lazy-array-delayed-action lazy-array))
             (let ((delayed-fuse (lazy-array-delayed-action lazy-array)))
               (loop for input in (delayed-fuse-inputs delayed-fuse)
                     when (subshapep relevant-shape (lazy-array-shape input)) do
                       (return-from ref
                         (ref input relevant-shape transformation)))))
           ;; Default: Construct a new reference.
           (make-lazy-array
            :shape shape
            :ntype (lazy-array-ntype lazy-array)
            :depth (1+ (lazy-array-depth lazy-array))
            :delayed-action
            (make-delayed-reshape
             :input lazy-array
             :transformation (add-transformation-constraints shape transformation)))))
      (ref input relevant-shape transformation))))

;;; We can turn each axis of the resulting shape that consists of a single
;;; element into an additional input constraint for the transformation.
;;; This augmentation is important, because the additional constraints can
;;; turn a previously non-invertible transformation invertible.
(defun add-transformation-constraints (shape transformation)
  (if (loop for range in (shape-ranges shape)
            for mask-entry across (transformation-input-mask transformation)
            never (and (size-one-range-p range)
                       (not mask-entry)))
      transformation
      (let ((input-mask (copy-seq (transformation-input-mask transformation))))
        (loop for range in (shape-ranges shape)
              for index from 0
              when (size-one-range-p range)
                do (setf (aref input-mask index)
                         (range-start range)))
        (compose-transformations
         transformation
         (make-transformation :input-mask input-mask)))))

(defun transform-lazy-array (lazy-array transformation)
  (declare (lazy-array lazy-array)
           (transformation transformation))
  (lazy-ref
   lazy-array
   (transform-shape (lazy-array-shape lazy-array) transformation)
   (invert-transformation transformation)))

(defun lazy-collapse (array)
  (let* ((lazy-array (lazy-array array))
         (shape (lazy-array-shape lazy-array)))
    (transform-lazy-array lazy-array (collapsing-transformation shape))))

