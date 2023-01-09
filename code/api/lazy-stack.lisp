;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-stack (axis array &rest more-arrays)
  (flet ((nth-array (n)
           (if (zerop n)
               array
               (nth (1- n) more-arrays))))
    (let* ((lazy-array (lazy-array array))
           (more-lazy-arrays (mapcar #'lazy-array more-arrays))
           (rank (lazy-array-rank lazy-array)))
      ;; Check that all supplied arrays have the same rank.
      (loop for lazy-array in more-lazy-arrays and n from 1 do
        (unless (= (lazy-array-rank lazy-array) rank)
          (error "~@<Cannot stack arrays with varying ranks, got ~S and ~S.~:@>"
                 (nth-array 0)
                 (nth-array n))))
      ;; Check that the axis is valid for the supplied arrays.
      (unless (and (integerp axis) (< -1 axis rank))
        (error "~@<Invalid stack axis ~S for the array ~S.~:@>"
               axis array))
      ;; Determine the step size.
      (let ((lazy-arrays (list* lazy-array more-lazy-arrays))
            (step nil)
            ;; For error reporting, we track the position of the first
            ;; array with more than one element along the specified AXIS.
            (pos nil))
        (loop for lazy-array in lazy-arrays
              for index from 0
              for range = (nth axis (shape-ranges (lazy-array-shape lazy-array)))
              unless (size-one-range-p range)
                do (if (null step)
                       (setf step (range-step range) pos index)
                       (unless (= step (range-step range))
                         (error "~@<Cannot stack arrays with varying step sizes, got ~S and ~S.~:@>"
                                (nth-array pos)
                                (nth-array index)))))
        ;; If there are no arrays with a size larger than one in the
        ;; specified axis, we pick a reasonable default.
        (when (null step)
          (setf step 1))
        ;; Now stack the arrays.
        (apply
         #'lazy-fuse
         (let ((position nil))
           (mapcar
            (lambda (lazy-array)
              (with-accessors ((start range-start)
                               (last range-last))
                  (nth axis (shape-ranges (lazy-array-shape lazy-array)))
                (cond ((null position)
                       (setf position (+ last step))
                       lazy-array)
                      ((= position start)
                       (setf position (+ last step))
                       lazy-array)
                      (t
                       (let* ((offsets (make-array rank :initial-element 0))
                              (offset (- position start)))
                         (setf (aref offsets axis)
                               offset)
                         (setf position
                               (+ last offset step))
                         (lazy-reshape lazy-array (make-transformation :offsets offsets)))))))
            lazy-arrays)))))))
