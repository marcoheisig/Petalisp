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
      #(+1 -1 +2 -2 +3 -3)))
  (compute
   (β (lambda (a b) (values a b)) #(3 2 1))
   (β (lambda (a b) (values b a)) #(3 2 1))))

(test fusion-test
  (compute
   (fuse* (reshape 0.0 (make-shape (range 2 4) (range 2 4)))
          (reshape 1.0 (make-shape (range 3 3) (range 3 3))))))

(test reference-test
  (compute
   (reshape #(1 2 3) (τ (i) ((- i)))) #(3 2 1))
  (compute
   (fuse*
    (reshape #2A((1 2 3) (4 5 6))
             (τ (i j) ((+ 2 i) (+ 3 j))))
    (reshape 9 (τ () (3 4))))))

(test multiple-arguments
  (compute 1 2 3 4 5 6 7 8 9 (α #'+ 5 5) (β #'+ #(1 2 3 4 1))))

(test indices-test
  (compute (indices #(5 6 7)))
  (let ((a (make-array '(2 3 4))))
    (compute (indices a 1))
    (compute (α #'+ (indices a 0) (indices a 1) (indices a 2)))))
