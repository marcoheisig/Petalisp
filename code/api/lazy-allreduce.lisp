;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-allreduce (function &rest arrays)
  (let ((lazy-arrays (lazy-broadcast-list-of-arrays arrays)))
    (loop until (zerop (shape-rank (lazy-array-shape (first lazy-arrays))))
          do (setf lazy-arrays
                   (multiple-value-list
                    (apply #'lazy-reduce function lazy-arrays))))
    (values-list lazy-arrays)))
