;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-broadcast-list-of-arrays (list-of-arrays)
  ;; As a first step, we create an alist whose keys are shapes, and whose
  ;; value is initially NIL and later set to a suitable transformation from
  ;; that shape to the common broadcast shape.
  (let ((lazy-arrays (mapcar #'lazy-array list-of-arrays))
        (alist '()))
    (loop for lazy-array in lazy-arrays do
      (let* ((shape (lazy-array-shape lazy-array))
             (entry (assoc shape alist :test #'shape=)))
        (when (null entry)
          (push (cons shape nil) alist))))
    (let* ((max-rank (loop for (shape) in alist maximize (shape-rank shape)))
           (ranges (make-array max-rank :initial-element (range 1))))
      (loop for (shape) in alist
            for rank = (shape-rank shape) do
              (loop for range in (shape-ranges shape)
                    for index from (- max-rank rank) do
                      (setf (aref ranges index)
                            (broadcast-ranges (aref ranges index) range))))
      (let ((broadcast-shape (make-shape (coerce ranges 'list))))
        (loop for entry in alist do
          (setf (cdr entry)
                (make-broadcast-transformation broadcast-shape (car entry))))
        (values
         (loop for lazy-array in lazy-arrays
               collect
               (lazy-ref
                lazy-array
                broadcast-shape
                (cdr (assoc (lazy-array-shape lazy-array) alist :test #'shape=))))
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

(defun lazy-broadcast-arrays (&rest arrays)
  (values-list
   (lazy-broadcast-list-of-arrays arrays)))

(defun lazy-broadcast-to (array shape-designator)
  (let* ((lazy-array (lazy-array array))
         (target-shape (shape-designator-shape shape-designator)))
    (lazy-ref
     lazy-array
     target-shape
     (make-broadcast-transformation target-shape (lazy-array-shape lazy-array)))))

(defun make-broadcast-transformation (input-shape output-shape)
  (multiple-value-bind (transformation broadcast-p select-p)
      (make-shape-transformation input-shape output-shape)
    (declare (ignore broadcast-p))
    (assert (not select-p))
    transformation))
