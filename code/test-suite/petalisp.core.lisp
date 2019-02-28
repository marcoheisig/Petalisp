;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sets

(test test-sets

  )

(test test-set-for-each
  )

(test test-set-difference
  )

(test test-set-elements
  )

(test test-set-emptyp
  )

(test test-set-equal
  )

(test test-set-contains
  )

(test test-set-intersection
  )

(test test-set-intersectionp
  )

(test test-set-subsetp
  )

(test test-set-size
  )

(test test-set-union
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

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
   (fuse (reshape (vector 4 5 6) (τ (i) ((+ i 3))))
         (vector 1 2 3)))
  (compute
   (fuse* (reshape 0.0 (~ 2 4 ~ 2 4))
          (reshape 1.0 (~ 3 ~ 3)))))

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
