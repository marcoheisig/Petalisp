;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

(in-suite petalisp)

(test application
  (check (α #'+ 2 3) 5)
  (check (α #'+ #(2 3 4) #(5 4 3)) #(7 7 7))
  (check (α #'+ #2A((1 2) (3 4)) #2A((4 3) (2 1))) #2A((5 5) (5 5)))
  (check (α #'floor #(1 2.5 1/2) 2) #(0 1 0) #(1 0.5 1/2)))

(test reduction
  (check (β #'+ #(1 2 3)) 6)
  (check (β #'+ #2A((1 2 3) (6 5 4))) #(7 7 7))
  (check (β (lambda (lmax lmin rmax rmin)
                            (values (max lmax rmax) (min lmin rmin)))
                          #(+1 -1 +2 -2 +3 -3)
                          #(+1 -1 +2 -2 +3 -3))
         3 -3))

(test fusion
  (check (fuse* (reshape 0.0 '((2 4) (2 4)))
                (reshape 1.0 '((3 3) (3 3))))
         #2A((0.0 0.0 0.0)
             (0.0 1.0 0.0)
             (0.0 0.0 0.0))))

(test reference
  (check (transform #(1 2 3) (τ (i) ((- i)))) #(3 2 1)))
