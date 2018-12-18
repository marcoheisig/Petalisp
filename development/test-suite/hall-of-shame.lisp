;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(test sum-of-pairs
  (let* ((size 10)
         (a (coerce-to-strided-array (make-array size :initial-element 0))))
    (compute
     (β #'+ (fuse (reshape a (~ 0 (- size 2))
                           (τ (i) (0 i)))
                  (reshape a (~ 1 (- size 1))
                           (τ (i) (1 (1- i)))))))))
