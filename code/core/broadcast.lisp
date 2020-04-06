;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun broadcast-list-of-arrays (list-of-arrays)
  ;; As a first step, we create an alist whose keys are shapes, and whose
  ;; value is initially NIL and later set to a suitable transformation from
  ;; that shape to the common broadcast shape.
  (let ((lazy-arrays (mapcar #'lazy-array list-of-arrays))
        (alist '()))
    (loop for lazy-array in list-of-arrays do
      (let* ((shape (shape lazy-array))
             (entry (assoc shape alist :test #'shape-equal)))
        (when (null entry)
          (push (cons shape nil) alist))))
    (let* ((max-rank (loop for (shape) in alist maximize (shape-rank shape)))
           (ranges (make-array max-rank :initial-element (range 0))))
      (loop for (shape) in alist
            for rank = (shape-rank shape) do
              (loop for range in (shape-ranges shape)
                    for index from (- max-rank rank) do
                      (setf (aref ranges index)
                            (broadcast-ranges (aref ranges index) range))))
      (let ((broadcast-shape (~l (coerce ranges 'list))))
        (loop for entry in alist do
          (setf (cdr entry)
                (make-broadcast-transformation broadcast-shape (car entry))))
        (values
         (loop for lazy-array in lazy-arrays
               collect
               (lazy-reshape
                lazy-array
                broadcast-shape
                (cdr (assoc (shape lazy-array) alist :test #'shape-equal))))
         broadcast-shape)))))

(defun broadcast-ranges (range-1 range-2)
  (cond ((size-one-range-p range-1) range-2)
        ((size-one-range-p range-2) range-1)
        ((= (range-size range-1)
            (range-size range-2))
         range-2)
        ((range-equal range-1 range-2) range-2)
        (t
         (error "~@<Cannot broadcast the ranges ~S and ~S.~:@>"
                range-1 range-2))))

(defun broadcast-arrays (&rest arrays)
  (values-list
   (broadcast-list-of-arrays arrays)))

(defun broadcast (array shape)
  (let* ((lazy-array (lazy-array array))
         (array-shape (shape lazy-array))
         (shape (shape shape)))
    (lazy-reshape
     lazy-array
     shape
     (make-broadcast-transformation shape array-shape))))
