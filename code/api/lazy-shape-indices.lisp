;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-shape-indices (shape &optional (axis 0))
  (let ((rank (shape-rank shape)))
    (unless (<= 0 axis (1- rank))
      (error "~@<Invalid axis ~A for a shape with rank ~D.~:@>" axis rank))
    (lazy-ref
     (make-range-immediate (nth axis (shape-ranges shape)))
     shape
     (make-transformation
      :input-rank rank
      :output-mask (vector axis)))))

