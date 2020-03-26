;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun broadcast (array shape)
  (let* ((lazy-array (lazy-array array))
         (array-shape (shape lazy-array))
         (shape (shape shape)))
    ;; Pick off the trivial case immediately.
    (if (shape-equal array-shape shape)
        lazy-array
        (multiple-value-bind (transformation broadcast-p select-p)
            (make-shape-transformation shape array-shape)
          (declare (ignore broadcast-p))
          (assert (not select-p))
          (lazy-rehape lazy-array shape transformation)))))

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

(define-modify-macro broadcast-ranges-f (range-2)
  broadcast-ranges)

(defun broadcast-arrays (&rest arrays)
  (values-list
   (broadcast-list-of-arrays arrays)))

(defun broadcast-list-of-arrays (list-of-arrays)
  (let* ((lazy-arrays (mapcar #'lazy-array list-of-arrays))
         (shapes (mapcar #'shape lazy-arrays))
         (rank (loop for shape in shapes maximize (shape-rank shape)))
         (broadcast-ranges '()))
    (loop for axis from (1- rank) downto 0 do
      (let ((broadcast-range (range 0)))
        (loop for shape in shapes do
          (let ((other-range (nth-broadcast-range shape rank axis)))
            (broadcast-ranges-f broadcast-range other-range)))
        (push broadcast-range broadcast-ranges)))
    (let ((broadcast-shape (~l broadcast-ranges)))
      (values
       (loop for lazy-array in lazy-arrays
             for shape in shapes
             collect (broadcast lazy-array broadcast-shape))
       broadcast-shape))))

;;; Pad SHAPE with leading one element ranges until it reaches
;;; BROADCAST-RANK.  Then, access the range corresponding to AXIS of the
;;; resulting padded shape.
(defun nth-broadcast-range (shape broadcast-rank axis)
  (declare (shape shape))
  (let* ((padding (- broadcast-rank (shape-rank shape)))
         (n (- axis padding)))
    (if (minusp n)
        (range 0)
        (nth n (shape-ranges shape)))))
