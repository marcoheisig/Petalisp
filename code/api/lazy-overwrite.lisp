;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-overwrite (array &rest more-arrays)
  (let* ((first (lazy-array array))
         (rank (lazy-array-rank first))
         (rest (mapcar #'lazy-array more-arrays))
         (lazy-arrays (list* first rest))
         (identity (identity-transformation (lazy-array-rank first)))
         (mismatches (remove rank rest :key #'lazy-array-rank)))
    (unless (null mismatches)
      (error "~@<Can only fuse arrays with equal rank.  The arrays ~
                 ~{~#[~;and ~S~;~S ~:;~S, ~]~} do not have the ~
                 same rank as the first argument ~S.~:@>"
             mismatches first))
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
