;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-sort (array predicate)
  (let* ((x (lazy-array array))
         (n (lazy-array-dimension x 0)))
    ;; The algorithm used here is Batcher's odd-even sort.
    (loop for p = 1 then (ash p 1) while (< p n) do
      (loop for k = p then (ash k -1) while (>= k 1) do
        (let* ((up (make-transformation :offsets (vector k)))
               (down (invert-transformation up))
               (divisor (ash p 1))
               (j0 (mod k p)))
          (multiple-value-bind (blocks rest) (floor (- n j0) (* 2 k))
            (setf x (apply
                     #'lazy-fuse
                     (lazy-reshape x 1 (~ 0 j0))
                     (lazy-reshape x 1
                      (let ((last (+ j0 (* 2 k blocks))))
                        (if (<= rest k)
                            (~ last n)
                            (~ (+ last (- rest k)) (+ last k)))))
                     (mapcan
                      (lambda (upper-shape)
                        (let* ((lower-shape (transform-shape upper-shape down))
                               (lower-indices (lazy-index-components lower-shape))
                               (lower-values (lazy-reshape x 1 lower-shape))
                               (upper-indices (lazy-reshape (lazy-index-components upper-shape) down))
                               (upper-values (lazy-reshape x 1 upper-shape down)))
                          (multiple-value-bind (lo hi)
                              (lazy-multiple-value 2
                               'typo:cswap
                               (lazy 'or
                                (lazy #'/=
                                 (lazy #'floor lower-indices divisor)
                                 (lazy #'floor upper-indices divisor))
                                (lazy predicate lower-values upper-values))
                               lower-values
                               upper-values)
                            (list lo (lazy-reshape hi up)))))
                      (if (< k blocks)
                          (loop for i from 0 below k
                                collect
                                (~ (+ j0 i k) n (* 2 k)))
                          (loop for j from (+ j0 k) by (* 2 k) below n
                                collect
                                (~ j (min (+ j k) n)))))))))))
    x))

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
