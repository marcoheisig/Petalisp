;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun stack (axis &rest arrays)
  (let ((lazy-arrays (mapcar #'lazy-array arrays))
        (stack-rank nil)
        (stack-width nil))
    ;; Loop over all arrays to validate the input and to determine a
    ;; suitable stack width.
    (loop for lazy-array in lazy-arrays and index from 0 do
      (let* ((shape (shape lazy-array))
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
            (cond ((null stack-width)
                   (setf stack-width (range-size range)))
                  ((= stack-width (range-size range))
                   (values))
                  (t
                   (error "~@<Cannot stack arrays with varying step sizes.~:@>")))))))
    ;; If there are no arrays with a size larger than one in the specified
    ;; axis, we pick a reasonable default.
    (when (null stack-width)
      (setf stack-width 1))
    ;; Now stack the arrays.
    (apply
     #'fuse
     (mapcar
      (let ((position nil))
        (lambda (lazy-array)
          (multiple-value-bind (start step end)
              (range-start-step-end (nth axis (shape-ranges (shape lazy-array))))
            (declare (ignore step))
            (cond ((null position)
                   (setf position (+ end stack-width))
                   lazy-array)
                  ((= position start)
                   (setf position (+ end stack-width))
                   lazy-array)
                  (t
                   (let ((offsets (make-array stack-rank :initial-element 0)))
                     (setf (aref offsets axis) (- position start))
                     (setf position (+ end stack-width))
                     (reshape lazy-array (make-transformation :offsets offsets))))))))
      lazy-arrays))))
