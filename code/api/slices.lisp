;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun slices (array range &optional (axis 0))
  (setf array (lazy-array array))
  (let ((rank (rank array))
        (ranges (shape-ranges (array-shape array))))
    (unless (< -1 axis rank)
      (error "~@<Invalid slices axis ~S for the array ~S.~:@>"
             axis array))
    (unless (null (range-difference-list range (nth axis ranges)))
      (error "~@<Invalid slices range ~S for the axis ~S of the array ~S~:@>"
             range axis array))
    (reshape
     array
     (make-shape
      (petalisp.utilities:with-collectors ((ranges collect-range))
        (do ((rest ranges (cdr rest))
             (pos 0 (1+ pos)))
            ((= pos axis)
             (collect-range range)
             (ranges (cdr rest)))
          (collect-range (car rest))))))))
