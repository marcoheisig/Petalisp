;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-multireduce (number-of-axes function &rest arrays)
  (multiple-value-bind (lazy-arrays shape)
      (lazy-broadcast-list-of-arrays arrays)
    (unless (and (integerp number-of-axes)
                 (<= 0 number-of-axes (shape-rank shape)))
      (error "~@<Invalid number of axes: ~S~:@>" number-of-axes))
    (loop repeat number-of-axes do
      (setf lazy-arrays
            (multiple-value-list
             (apply #'lazy-reduce function lazy-arrays))))
    (values-list lazy-arrays)))
