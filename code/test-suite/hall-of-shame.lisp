;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(test sum-of-pairs
  (let* ((size 10)
         (a (coerce-to-lazy-array (make-array size :initial-element 0))))
    (compute
     (β #'+ (fuse (reshape a (~ 0 (- size 2))
                           (τ (i) (0 i)))
                  (reshape a (~ 1 (- size 1))
                           (τ (i) (1 (1- i)))))))))

(test reduction-of-fusions
  (compute
   (β #'+ (fuse #(1 2 3)
                (reshape #(4 5 6) (τ (i) ((+ i 3))))))))
