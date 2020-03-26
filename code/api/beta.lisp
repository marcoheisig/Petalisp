;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

#+(or)
(defun β (function &rest arrays &aux (n-values (length arrays)))
  (multiple-value-bind (inputs input-shape)
      (broadcast-list-of-arrays arrays)
    (cond ((null input-shape)
           (empty-arrays n-values))
          ((zerop (shape-rank input-shape))
           (empty-arrays n-values))
          (t
           (β-aux function n-values input-shape inputs)))))

#+(or)
(defun β-aux (function n-values shape inputs)
  (trivia:ematch shape
    ((~ _ ~l _)
     )))

(defun β* (f z x &optional axis)
  (cond ((empty-array-p x) z)
        ((typep axis 'rank)
         (β f (move-axis-to-front x axis)))
        ((null axis)
         (loop repeat (rank x)
               do (setf x (β f x))
               finally (return x)))
        (t (error "Not a valid axis: ~S" axis))))
