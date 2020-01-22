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
