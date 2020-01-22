;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(define-test range-test
  ;; Range constructors
  (is (rangep (range 1 0 1)))
  (is (rangep (apply #'range (list 1 2 3))))
  (signals error (range 1 0 99))
  ;; Range operations
  (labels ((test-range (range)
             (declare (notinline range-start-step-end size-one-range-p range-end))
             (is (rangep range))
             (with-output-to-string (stream)
               (print range stream))
             (multiple-value-bind (start step end)
                 (range-start-step-end range)
               (is (= start (range-start range)))
               (is (= step (range-step range)))
               (is (= end (range-end range)))
               (is (<= (range-start range)
                       (range-end range))))
             (if (= 1 (range-size range))
                 (is (size-one-range-p range))
                 (is (not (size-one-range-p range))))
             (is (range-equal range range))
             (is (range-equal range (multiple-value-call #'range (range-start-step-end range))))
             (if (size-one-range-p range)
                 (is (= (range-start range)
                        (range-end range)))
                 (is (/= (range-start range)
                         (range-end range))))
             (map-range
              (lambda (index)
                (is (range-contains range index)))
              range)
             (is (not (range-contains range (1- (range-start range)))))
             (is (not (range-contains range (1+ (range-end range))))))
           (test-range-pair (range-1 range-2)
             (test-range range-1)
             (test-range range-2)
             (let ((intersection-1 (range-intersection range-1 range-2))
                   (intersection-2 (range-intersection range-1 range-2))
                   (differences-1 (range-difference-list range-1 range-2))
                   (differences-2 (range-difference-list range-2 range-1)))
               (when (range-intersectionp range-1 range-2)
                 (is (range-equal intersection-1 intersection-2))
                 (is (= (reduce #'+ differences-1 :key #'range-size)
                        (- (range-size range-1)
                           (range-size intersection-1))))
                 (is (= (reduce #'+ differences-2 :key #'range-size)
                        (- (range-size range-2)
                           (range-size intersection-2))))))))
    (test-range-pair (range 0) (range 0))
    (test-range-pair (range 0) (range 1))
    (test-range-pair (range -1) (range 1))
    (test-range-pair (range 0 2) (range 1))
    (test-range-pair (range 0 99) (range 1 100))
    (test-range-pair (range 0 2 99) (range 3 100))
    (test-range-pair (range 0 3 50) (range 55 5 100))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Networks

(define-test network-test
  (let* ((shape (~ 0 9))
         (x1 (make-parameter :x1 :shape shape :element-type 'double-float))
         (x2 (make-parameter :x2 :shape shape :element-type 'double-float))
         (v1 (α #'+
                (α #'coerce (α #'log x1) 'double-float)
                (α #'* x1 x2)
                (α #'sin x2)))
         (gradient-fn (differentiator (list v1) (list :g1)))
         (network (make-network v1))
         (gradient-network (make-network
                            (funcall gradient-fn x1)
                            (funcall gradient-fn x2))))
    (call-network network :x1 5d0 :x2 1d0)
    (call-network gradient-network :x1 1d0 :x2 1d0 :g1 1d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Full Programs

(define-test application-test
  (compute
   (α #'+ 2 3))
  (compute
   (α #'+ #(2 3 4) #(5 4 3)))
  (compute
   (α #'+ #2A((1 2) (3 4)) #2A((4 3) (2 1))))
  (compute
   (α #'floor #(1 2.5 1/2) 2)))

(define-test reduction-test
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

(define-test fusion-test
  (compute
   (fuse (reshape (vector 4 5 6) (τ (i) ((+ i 3))))
         (vector 1 2 3)))
  (compute
   (fuse* (reshape 0.0 (~ 2 4 ~ 2 4))
          (reshape 1.0 (~ 3 ~ 3)))))

(define-test reshape-test
  (compute (reshape 4 (~ 0 4)))
  (compute (reshape #(1 2 3) (τ (i) ((- i)))) #(3 2 1))
  (compute (reshape #(1 2 3 4) (~ 1 2)))
  (compute (reshape (indices (~ 1 9)) (~ 0 2 ~ 0 2)))
  (compute (reshape #2A((1 2) (3 4)) (τ (i j) (j i))))
  (compute (reshape #(1 2 3 4) (~ 1 2) (~ 0 1 ~ 0 1)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'reshape (indices (~ 1 100)) shapes)))
   (list (~ 0 4 ~ 0 4 ~ 0 3)
         (~ 0 1 ~ 0 4 ~ 0 0 ~ 0 1 ~ 0 4)
         (~ 1 2 ~ 1 5 ~ 1 2 ~ 1 5)
         (~ 1 2 3 ~ 1 2 9 ~ 1 2 3 ~ 1 2 9)
         (~ 0 99)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'reshape (indices (~ 1 200)) shapes)))
   (list (~ 0 1 ~ 0 4 ~ 0 4 ~ 0 3)
         (~ 0 1 ~ 0 1 ~ 0 4 ~ 0 0 ~ 0 1 ~ 0 4)
         (~ 0 1 ~ 0 99)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'reshape (indices (~ 1 200)) shapes)))
   (list (~ 0 4 ~ 0 4 ~ 0 3 ~ 0 1)
         (~ 0 1 ~ 0 4 ~ 0 0 ~ 0 1 ~ 0 4 ~ 0 1)
         (~ 0 99 ~ 0 1)))
  (compute
   (fuse*
    (reshape #2A((1 2 3) (4 5 6)) (τ (i j) ((+ 2 i) (+ 3 j))))
    (reshape 9 (τ () (3 4)))))
  (signals error
    (compute (reshape #(1 2 3 4) (~ -1 3)))))

(define-test multiple-arguments
  (compute 1 2 3 4 5 6 7 8 9 (α #'+ 5 5) (β #'+ #(1 2 3 4 1))))

(define-test indices-test
  (compute (indices #(5 6 7)))
  (let ((a (make-array '(2 3 4))))
    (compute (indices a 1))
    (compute (α #'+ (indices a 0) (indices a 1) (indices a 2)))))


(define-test sum-of-pairs
  (let* ((size 10)
         (a (lazy-array (make-array size :initial-element 0))))
    (compute
     (β #'+ (fuse (reshape a (~ 0 (- size 2))
                           (τ (i) (0 i)))
                  (reshape a (~ 1 (- size 1))
                           (τ (i) (1 (1- i)))))))))

(define-test reduction-of-fusions
  (compute
   (β #'+ (fuse #(1 2 3)
                (reshape #(4 5 6) (τ (i) ((+ i 3))))))))
