;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-stack (axis &rest arrays)
  (unless (typep axis 'rank)
    (error "Not a valid array axis: ~S." axis))
  (let ((lazy-arrays (mapcar #'lazy-array arrays))
        (stack-rank nil)
        (stack-step nil))
    ;; Loop over all arrays to validate the input and to determine a
    ;; suitable stack width.
    (loop for lazy-array in lazy-arrays
          for index from 0 do
      (let* ((shape (array-shape lazy-array))
             (rank (shape-rank shape)))
        ;; Determine the stack rank.
        (cond ((null stack-rank)
               (setf stack-rank rank))
              ((= rank stack-rank)
               (values))
              (t
               (error "~@<Cannot stack arrays with varying ranks.~:@>")))
        ;; Determine the stack axis.
        (unless (< -1 axis rank)
          (error "~@<Invalid stack axis ~S for the array ~S.~:@>"
                 axis (nth index arrays)))
        (let ((range (nth axis (shape-ranges shape))))
          (unless (size-one-range-p range)
            (cond ((null stack-step)
                   (setf stack-step (range-step range)))
                  ((= stack-step (range-step range))
                   (values))
                  (t
                   (error "~@<Cannot stack arrays with varying step sizes.~:@>")))))))
    ;; If there are no arrays with a size larger than one in the specified
    ;; axis, we pick a reasonable default.
    (when (null stack-step)
      (setf stack-step 1))
    ;; Now stack the arrays.
    (apply
     #'lazy-fuse
     (let ((position nil))
       (mapcar
        (lambda (lazy-array)
          (with-accessors ((start range-start)
                           (last range-last))
              (nth axis (shape-ranges (array-shape lazy-array)))
            (cond ((null position)
                   (setf position (+ last stack-step))
                   lazy-array)
                  ((= position start)
                   (setf position (+ last stack-step))
                   lazy-array)
                  (t
                   (let* ((offsets (make-array stack-rank :initial-element 0))
                          (offset (- position start)))
                     (setf (aref offsets axis)
                           offset)
                     (setf position
                           (+ last offset stack-step))
                     (lazy-reshape lazy-array (make-transformation :offsets offsets)))))))
        lazy-arrays)))))
