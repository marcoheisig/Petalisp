;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(test application-test
  (compute
   (α #'+ 2 3))
  (compute
   (α #'+ #(2 3 4) #(5 4 3)))
  (compute
   (α #'+ #2A((1 2) (3 4)) #2A((4 3) (2 1))))
  (compute
   (α #'floor #(1 2.5 1/2) 2)))

(test reduction-test
  (compute
   (β #'+ #(1 2 3)))
  (compute
   (β #'+ #2A((1 2 3) (6 5 4))))
  (compute
   (β (lambda (lmax lmin rmax rmin)
        (values (max lmax rmax) (min lmin rmin)))
      #(+1 -1 +2 -2 +3 -3)
      #(+1 -1 +2 -2 +3 -3))))

(test fusion-test
  (compute
   (fuse* (reshape 0.0 '((2 4) (2 4)))
          (reshape 1.0 '((3 3) (3 3))))))

(test reference-test
  (compute
   (reshape #(1 2 3) (τ (i) ((- i)))) #(3 2 1)))
