;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-drop-axes (array &rest axes)
  (let* ((lazy-array (lazy-array array))
         (shape (lazy-array-shape lazy-array))
         (input-rank (shape-rank shape))
         (input-mask (make-array input-rank :initial-element nil))
         (output-mask (make-array input-rank)))
    (dolist (axis axes)
      (unless (< -1 axis input-rank)
        (error "~@<Invalid axis ~D for array ~S.~:@>"
               axis array))
      (let ((range (nth axis (shape-ranges shape))))
        (unless (size-one-range-p range)
          (error "~@<Can only drop axes with size one, but the axis ~D ~
                 of the array ~S has a size of ~D.~:@>"
                 axis array (range-size range)))
        (setf (svref input-mask axis) (range-start range))))
    (let ((output-index 0))
      (loop for input-index below input-rank do
        (when (null (svref input-mask input-index))
          (setf (svref output-mask output-index) input-index)
          (incf output-index)))
      (lazy-reshape
       lazy-array
       (make-transformation
        :input-mask input-mask
        :output-mask (subseq output-mask 0 output-index))))))
