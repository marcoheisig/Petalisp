;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun fuse (&rest inputs)
  (lazy-fuse inputs))

(defun fuse* (&rest inputs)
  (let ((lazy-arrays (mapcar #'lazy-array inputs)))
    (unless (petalisp.utilities:identical lazy-arrays :test #'= :key #'rank)
      (error "~@<Can only fuse arrays with equal rank.  The arrays ~
                 ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this ~
                 requirement.~:@>"
             inputs)))
  (let* ((lazy-arrays (mapcar #'lazy-array inputs))
         (identity (identity-transformation (rank (first lazy-arrays)))))
    (lazy-fuse
     (mapcar
      (lambda (fragment)
        (destructuring-bind (shape . bitmask) fragment
          (lazy-reshape
           (nth (1- (integer-length bitmask)) lazy-arrays)
           shape
           identity)))
      (subdivide lazy-arrays)))))
