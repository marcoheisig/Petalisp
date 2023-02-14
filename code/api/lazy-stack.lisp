;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-stack (axis array &rest more-arrays)
  (let* ((arrays (list* array more-arrays))
         (lazy-array (lazy-array array))
         (more-lazy-arrays (mapcar #'lazy-array more-arrays))
         (lazy-arrays (list* lazy-array more-lazy-arrays))
         (rank (lazy-array-rank lazy-array))
         (step 1))
    ;; Check that the axis is valid.
    (unless (and (integerp axis) (< -1 axis rank))
      (error "~@<Invalid stack axis ~S for the array ~S.~:@>"
             axis array))
    ;; Check that all supplied arrays have the same rank, and the same
    ;; shape in all but the specified AXIS.
    (loop for other-lazy-array in more-lazy-arrays for index from 0 do
      (unless (= (lazy-array-rank other-lazy-array) rank)
        (error "~@<Cannot stack arrays with varying ranks, got ~S and ~S.~:@>"
               array (nth index arrays)))
      (loop for range1 in (lazy-array-ranges lazy-array)
            for range2 in (lazy-array-ranges other-lazy-array)
            for pos below rank
            when (/= pos axis)
              do (unless (range= range1 range2)
                   (error "~@<Arrays being stacked must only differ in the specified axis, ~
                              but the arrays ~S and ~S also differ in axis ~D.~:@>"
                          array (nth index arrays) pos))))
    ;; Determine the step size.
    (loop for lazy-array in lazy-arrays
          for index from 0
          ;; For error reporting, we track the position of the first array
          ;; with more than one element along AXIS.
          with pos = nil
          do (let ((range (shape-range (lazy-array-shape lazy-array) axis)))
               (when (> (range-size range) 1)
                 (unless (null pos)
                   (unless (= step (range-step range))
                     (error "~@<Cannot stack arrays with varying step sizes, got ~S and ~S.~:@>"
                            (nth pos (list* array more-arrays))
                            (nth index (list* array more-arrays)))))
                 (setf step (range-step range))
                 (setf pos index))))
    ;; Now stack the arrays.
    (let* ((inputs (remove 0 lazy-arrays :key #'lazy-array-size))
           (transformations
             (loop for input in inputs
                   for range = (lazy-array-range input axis)
                   for start = (range-start range)
                   for position = start then (+ position increment)
                   for increment = (* (range-size range) step)
                   for offsets = (make-array rank :initial-element 0)
                   do (setf (aref offsets axis) (- position start))
                   collect (make-transformation :offsets offsets))))
      (if (null inputs)
          lazy-array
          (apply
           #'lazy-fuse
           (mapcar #'lazy-reshape inputs transformations))))))
