;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-fuse (array &rest more-arrays)
  (let ((first (lazy-array array))
        (rest (mapcar #'lazy-array more-arrays)))
    ;; No need to fuse when only a single array is supplied.
    (when (null rest)
      (return-from lazy-fuse first))
    (let ((rank (lazy-array-rank first))
          (lazy-arrays (list* first rest)))
      ;; Check that all lazy arrays have the same rank.
      (dolist (lazy-array rest)
        (unless (= (lazy-array-rank lazy-array) rank)
          (error
           "~@<Can only fuse arrays with ~
               equal rank. The arrays ~A and ~A ~
               violate this requirement.~:@>"
           first lazy-array)))
      ;; Check that all lazy arrays have a pairwise disjoint shape.
      (alexandria:map-combinations
       (lambda (lazy-arrays)
         (destructuring-bind (lazy-array-1 lazy-array-2) lazy-arrays
           (when (shape-intersectionp
                  (lazy-array-shape lazy-array-1)
                  (lazy-array-shape lazy-array-2))
             (error "~@<Can only fuse disjoint shapes, ~
                        but the arrays ~S and the shape ~S have the ~
                        common subshape ~S.~:@>"
                    lazy-array-1
                    lazy-array-2
                    (shape-intersection
                     (lazy-array-shape lazy-array-1)
                     (lazy-array-shape lazy-array-2))))))
       lazy-arrays :length 2 :copy nil)
      (let ((shape (apply #'fuse-shapes (mapcar #'lazy-array-shape lazy-arrays)))
            (ntype (reduce #'typo:ntype-union lazy-arrays :key #'lazy-array-ntype)))
        ;; Check that the predicted result shape is valid.
        (unless (= (reduce #'+ lazy-arrays :key #'lazy-array-size)
                   (shape-size shape))
          (error "~@<Cannot fuse the arrays ~
                     ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                 lazy-arrays))
        ;; Optimization: If the content of the fusion is predicted to be a
        ;; constant, we replace the entire fusion by a reference to that
        ;; constant.
        (when (typo:eql-ntype-p ntype)
          (return-from lazy-fuse
            (lazy-ref (lazy-array-from-scalar (typo:eql-ntype-object ntype))
             shape
             (make-transformation
              :input-rank (shape-rank shape)
              :output-rank 0))))
        ;; Optimization: If the fusion is equivalent to a broadcasting
        ;; reference (i.e., when all inputs are lazy reshapes of the same
        ;; lazy array, and all transformations of those reshapes are
        ;; similar), we replace the entire fusion by such a broadcasting
        ;; reference.
        (when (delayed-reshape-p (lazy-array-delayed-action first))
          (let* ((delayed-reshape (lazy-array-delayed-action first))
                 (first-input (delayed-reshape-input delayed-reshape))
                 (first-transformation (delayed-reshape-transformation delayed-reshape))
                 (first-output-mask (transformation-output-mask first-transformation))
                 (first-scalings (transformation-scalings first-transformation))
                 (first-offsets (transformation-offsets first-transformation)))
            (when (loop for lazy-array in rest
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
                 shape
                 (make-transformation
                  :input-rank (transformation-input-rank first-transformation)
                  :output-mask first-output-mask
                  :scalings first-scalings
                  :offsets first-offsets))))))
        ;; Default: Create a lazy array that performs the specified fusion.
        (make-lazy-array
         :delayed-action (make-delayed-fuse :inputs lazy-arrays)
         :shape shape
         :ntype ntype
         :depth (1+ (maxdepth lazy-arrays)))))))


