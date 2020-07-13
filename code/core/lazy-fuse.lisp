;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-fuse (arrays)
  (let ((lazy-arrays
          (loop for array in arrays
                unless (empty-array-p array)
                  collect (lazy-array array))))
    ;; Handle the case of zero supplied lazy arrays.
    (if (null lazy-arrays)
        (empty-array)
        (let ((first (first lazy-arrays))
              (rest (rest lazy-arrays)))
          ;; Optimize the case of fusing exactly one lazy array.
          (if (null rest)
              first
              (let ((rank (rank first)))
                ;; Check that all lazy arrays have the same rank.
                (dolist (lazy-array rest)
                  (unless (= (rank lazy-array) rank)
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
                            (shape lazy-array-1)
                            (shape lazy-array-2))
                       (error "~@<Can only fuse disjoint shapes, ~
                         but the arrays ~S and the shape ~S have the ~
                         common subshape ~S.~:@>"
                              lazy-array-1
                              lazy-array-2
                              (shape-intersection
                               (shape lazy-array-1)
                               (shape lazy-array-2))))))
                 lazy-arrays :length 2 :copy nil)
                (let ((shape (%make-shape
                              (apply #'mapcar #'fuse-ranges
                                     (mapcar (alexandria:compose #'shape-ranges #'shape)
                                             lazy-arrays))
                              rank))
                      (ntype (reduce #'petalisp.type-inference:ntype-union
                                     lazy-arrays
                                     :key #'element-ntype)))
                  ;; Check that the predicted result shape is valid.
                  (unless (= (reduce #'+ lazy-arrays :key #'total-size)
                             (shape-size shape))
                    (error "~@<Cannot fuse the arrays ~
                             ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                           lazy-arrays))
                  ;; If the content of the fusion is predicted to be a
                  ;; constant, we replace the entire fusion by a reference to
                  ;; that constant.  Otherwise, we create a regular lazy-fuse
                  ;; object.
                  (if (petalisp.type-inference:eql-ntype-p ntype)
                      (lazy-reshape
                       (make-scalar-immediate ntype)
                       shape
                       (make-transformation
                        :input-rank (shape-rank shape)
                        :output-rank 0))
                      (make-instance 'lazy-fuse
                        :ntype ntype
                        :inputs lazy-arrays
                        :shape shape)))))))))


