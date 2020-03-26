;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun slice (array index &optional (axis 0))
  (setf array (lazy-array array))
  (let ((rank (rank array))
        (ranges (shape-ranges (shape array))))
    (unless (< -1 axis rank)
      (error "~@<Invalid slice axis ~S for the array ~S.~:@>"
             axis array))
    (unless (range-contains (nth axis ranges) index)
      (error "~@<Invalid slice index ~S for the axis ~S of the array ~S~:@>"
             index axis array))
    (lazy-rehape
     array
     (~l
      (petalisp.utilities:with-collectors ((ranges collect-range))
        (do ((rest ranges (cdr rest))
             (pos 0 (1+ pos)))
            ((= pos axis) (ranges (cdr rest)))
          (collect-range (car rest)))))
     (make-transformation
      :input-rank (1- rank)
      :output-mask
      (loop for pos below rank
            collect
            (cond ((< pos axis) pos)
                  ((= pos axis) nil)
                  ((> pos axis) (1- pos))))
      :offsets
      (loop for pos below rank
            collect
            (if (= pos axis) index 0))))))
