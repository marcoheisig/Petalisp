;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-sort (array predicate &key (key 'identity))
  (let* ((x (lazy-array array))
         (d (funcall (deflater 1) (lazy-array-shape x)))
         (r (invert-transformation d))
         (x (lazy-reshape x d))
         (y (lazy key x)))
    (when (zerop (lazy-array-rank x))
      (error "~@<Cannot sort arrays of rank zero.~:@>"))
    (multiple-value-bind (x y)
        (if (eql x y)
            (lazy-valuesort x predicate)
            (lazy-keysort x y predicate))
      (values
       (lazy-reshape x r)
       (lazy-reshape y r)))))

(defun lazy-valuesort (x predicate)
  (let ((i (lazy-index-components x))
        (n (lazy-array-dimension x 0)))
    (loop for p = 1 then (ash p 1) while (< p n) do
      (loop for k = p then (ash k -1) while (>= k 1) do
        (let* ((up (make-transformation :offsets (vector k)))
               (down (invert-transformation up))
               (divisor (ash p 1)))
          (multiple-value-bind (inactive-shapes upper-shapes)
              (batcher-odd-even-shapes n p k)
            (let ((xparts '()))
              (dolist (inactive-shape inactive-shapes)
                (push (lazy-reshape x inactive-shape) xparts))
              (dolist (upper-shape upper-shapes)
                (let* ((lower-shape (transform-shape upper-shape down))
                       (xlo (lazy-reshape x lower-shape))
                       (xhi (lazy-reshape x upper-shape down))
                       (ilo (lazy-reshape i lower-shape))
                       (ihi (lazy-reshape i upper-shape down)))
                  (multiple-value-bind (xlo xhi)
                      (lazy-multiple-value 2 'typo:cswap
                       (lazy 'or
                        (lazy #'/=
                         (lazy #'floor ilo divisor)
                         (lazy #'floor ihi divisor))
                        (lazy predicate xlo xhi))
                       xlo xhi)
                    (push xlo xparts)
                    (push (lazy-reshape xhi up) xparts))))
              (setf x (apply #'lazy-fuse xparts)))))))
    (values x x)))

(defun lazy-keysort (x y predicate)
  (let ((i (lazy-index-components x))
        (n (lazy-array-dimension x 0)))
    (loop for p = 1 then (ash p 1) while (< p n) do
      (loop for k = p then (ash k -1) while (>= k 1) do
        (let* ((up (make-transformation :offsets (vector k)))
               (down (invert-transformation up))
               (divisor (ash p 1)))
          (multiple-value-bind (inactive-shapes upper-shapes)
              (batcher-odd-even-shapes n p k)
            (let ((xparts '())
                  (yparts '()))
              (dolist (inactive-shape inactive-shapes)
                (push (lazy-reshape x inactive-shape) xparts)
                (push (lazy-reshape y inactive-shape) yparts))
              (dolist (upper-shape upper-shapes)
                (let* ((lower-shape (transform-shape upper-shape down))
                       (xlo (lazy-reshape x lower-shape))
                       (ylo (lazy-reshape y lower-shape))
                       (xhi (lazy-reshape x upper-shape down))
                       (yhi (lazy-reshape y upper-shape down))
                       (ilo (lazy-reshape i lower-shape))
                       (ihi (lazy-reshape i upper-shape down)))
                  (multiple-value-bind (xlo ylo xhi yhi)
                      (lazy-multiple-value 4 'typo:cswap2
                       (lazy 'or
                        (lazy #'/=
                         (lazy #'floor ilo divisor)
                         (lazy #'floor ihi divisor))
                        (lazy predicate ylo yhi))
                       xlo ylo xhi yhi)
                    (push xlo xparts)
                    (push ylo yparts)
                    (push (lazy-reshape xhi up) xparts)
                    (push (lazy-reshape yhi up) yparts))))
              (setf x (apply #'lazy-fuse xparts))
              (setf y (apply #'lazy-fuse yparts)))))))
    (values x y)))

(defun batcher-odd-even-shapes (n p k)
  (let ((j0 (mod k p)))
    (multiple-value-bind (blocks rest) (floor (- n j0) (* 2 k))
      (values
       (list
        (~ 0 j0)
        (let ((last (+ j0 (* 2 k blocks))))
          (if (<= rest k)
              (~ last n)
              (~ (+ last (- rest k)) (+ last k)))))
       (if (< k blocks)
           (loop for i from 0 below k
                 collect (~ (+ j0 i k) n (* 2 k)))
           (loop for j from (+ j0 k) by (* 2 k) below n
                 collect (~ j (min (+ j k) n))))))))

;;; I'm leaving a naive implementation of Batcher's algorithm here as a
;;; comment, because it is the basis of the parallel implementation above.  In
;;; the parallel version, the two inner loops over j and i have been replaced
;;; by one large invocation of LAZY-FUSE, and the longer of the two inner loops
;;; is used to construct shapes of all the upper halves of each pairwise
;;; comparison.
#+(or)
(defun odd-even-sort (vector predicate)
  (let ((n (length vector)))
    ;; The algorithm used here is Batcher's odd-even sort.
    (loop for p = 1 then (ash p 1) while (< p n) do
      (loop for k = p then (ash k -1) while (>= k 1) do
        (loop for j from (mod k p) below (- n k) by (* 2 k) do
          (loop for i below (min k (- n j k)) do
            (when (= (floor (+ i j) (* p 2))
                     (floor (+ i j k) (* p 2)))
              (symbol-macrolet ((a (aref vector (+ i j)))
                                (b (aref vector (+ i j k))))
                (format t "~&p=~D k=~D j=~D i=~D Swap X[~D]=~A with X[~D]=~A~%"
                        p k j i (+ i j) a (+ i j k) b)
                (unless (funcall predicate a b)
                  (rotatef a b))))))))
    vector))
