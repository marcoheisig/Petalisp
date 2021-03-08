;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-overwrite (&rest inputs)
  (let* ((lazy-arrays (mapcar #'lazy-array inputs))
         (identity (identity-transformation (rank (first lazy-arrays)))))
    (unless (petalisp.utilities:identical lazy-arrays :test #'= :key #'rank)
      (error "~@<Can only fuse arrays with equal rank.  The arrays ~
                 ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this ~
                 requirement.~:@>"
             inputs))
    (apply
     #'lazy-fuse
     (mapcar
      (lambda (fragment)
        (destructuring-bind (shape . bitmask) fragment
          (lazy-ref
           (nth (1- (integer-length bitmask)) lazy-arrays)
           shape
           identity)))
      (subdivide-arrays lazy-arrays)))))
