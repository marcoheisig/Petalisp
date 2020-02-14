;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defvar *backend* (make-native-backend))

(defun β* (f z x &optional axis)
  (cond ((empty-array-p x) z)
        ((typep axis 'rank)
         (β f (move-axis-to-front x axis)))
        ((null axis)
         (loop repeat (rank x)
               do (setf x (β f x))
               finally (return x)))
        (t (error "Not a valid axis: ~S" axis))))

(declaim (inline vectorize))
(defun vectorize (function &optional (arity 1))
  (lambda (&rest args)
    (apply #'α* arity function args)))

(defun collapse (array)
  (let ((lazy-array (lazy-array array)))
    (reshape lazy-array (collapsing-transformation (shape lazy-array)))))

(defun flatten (array)
  (reshape array (~ 0 (1- (shape-size (shape array))))))

(defun slice (array index &optional (axis 0))
  (setf array (lazy-array array))
  (let ((rank (rank array))
        (ranges (shape-ranges (shape array))))
    (unless (< -1 axis rank)
      (error "~@<Invalid slice axis ~S for the array ~S.~:@>"
             axis array))
    (unless (range-contains (first ranges) index)
      (error "~@<Invalid slice index ~S for the array ~S~:@>"
             index array))
    (make-reference
     array
     (make-shape
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
