;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(define-test range-test
  ;; Range constructors
  (is (rangep (range 1 1 0)))
  (is (rangep (apply #'range (list 1 3 2))))
  (signals error (range 1 99 0))
  ;; Range operations
  (labels ((test-range (range)
             (declare (notinline size-one-range-p range-end))
             (is (rangep range))
             (with-output-to-string (stream)
               (print range stream))
             (if (= 1 (range-size range))
                 (is (size-one-range-p range))
                 (is (not (size-one-range-p range))))
             (is (range-equal range range))
             (cond ((empty-range-p range)
                    (is (zerop (range-size range)))
                    (return-from test-range))
                   ((size-one-range-p range)
                    (is (= (range-start range)
                           (range-last range))))
                   (t
                    (is (/= (range-start range)
                            (range-last range)))))
             (map-range
              (lambda (index)
                (is (range-contains range index)))
              range)
             (is (not (range-contains range (1- (range-start range)))))
             (is (not (range-contains range (range-end range)))))
           (test-range-pair (range1 range2)
             (test-range range1)
             (test-range range2)
             (let ((intersection1 (range-intersection range1 range2))
                   (intersection2 (range-intersection range1 range2))
                   (differences1 (range-difference-list range1 range2))
                   (differences2 (range-difference-list range2 range1)))
               (when (range-intersectionp range1 range2)
                 (is (range-equal intersection1 intersection2))
                 (is (= (reduce #'+ differences1 :key #'range-size)
                        (- (range-size range1)
                           (range-size intersection1))))
                 (is (= (reduce #'+ differences2 :key #'range-size)
                        (- (range-size range2)
                           (range-size intersection2))))))))
    (test-range-pair (range 0) (range 0))
    (test-range-pair (range 1) (range 1))
    (test-range-pair (range 0) (range 1))
    (test-range-pair (range 1) (range 1))
    (test-range-pair (range 0 2) (range 1))
    (test-range-pair (range 0 99) (range 1 100))
    (test-range-pair (range 0 99 2) (range 3 100))
    (test-range-pair (range 0 50 3) (range 55 100 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Networks and Automatic Differentiation

(define-test network-test
  (let* ((shape (~ 10))
         (x1 (make-instance 'parameter :shape shape :element-type 'double-float))
         (x2 (make-instance 'parameter :shape shape :element-type 'double-float))
         (v1 (α #'+
                (α #'coerce (α #'log x1) 'double-float)
                (α #'* x1 x2)
                (α #'sin x2)))
         (network
           (make-network v1))
         (g1 (make-instance 'parameter
               :shape (array-shape v1)
               :element-type (element-type v1)))
         (gradient-fn (differentiator (list v1) (list g1)))
         (gradient-network
           (make-network
            (funcall gradient-fn x1)
            (funcall gradient-fn x2))))
    (call-network network x1 5d0 x2 1d0)
    (call-network gradient-network x1 1d0 x2 1d0 g1 1d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Full Programs

(define-test lazy-map-test
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
   (fuse* (reshape 0.0 (~ 2 5 ~ 2 5))
          (reshape 1.0 (~ 3 4 ~ 3 4)))))

(define-test reshape-test
  (compute (reshape 4 (~ 5)))
  (compute (reshape #(1 2 3) (τ (i) ((- i)))) #(3 2 1))
  (compute (reshape #(1 2 3 4) (~ 1 3)))
  (compute (reshape (shape-indices (~ 1 10)) (~ 3 ~ 3)))
  (compute (reshape #2A((1 2) (3 4)) (τ (i j) (j i))))
  (compute (reshape #(1 2 3 4) (~ 1 3) (~ 0 2 ~ 0 2)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'reshape (shape-indices (~ 1 101)) shapes)))
   (list (~ 0 5 ~ 0 5 ~ 0 4)
         (~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5)
         (~ 1 3 ~ 1 6 ~ 1 3 ~ 1 6)
         (~ 1 4 2 ~ 1 10 2 ~ 1 4 2 ~ 1 10 2)
         (~ 100)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'reshape (shape-indices (~ 1 201)) shapes)))
   (list (~ 0 2 ~ 0 5 ~ 0 5 ~ 0 4)
         (~ 0 2 ~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5)
         (~ 0 2 ~ 0 100)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'reshape (shape-indices (~ 1 201)) shapes)))
   (list (~ 0 5 ~ 0 5 ~ 0 4 ~ 0 2)
         (~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5 ~ 0 2)
         (~ 0 100 ~ 0 2)))
  (compute
   (fuse*
    (reshape #2A((1 2 3) (4 5 6)) (τ (i j) ((+ 2 i) (+ 3 j))))
    (reshape 9 (τ () (3 4)))))
  (signals error
    (compute (reshape #(1 2 3 4) (~ -1 4)))))

(define-test multiple-arguments
  (compute 1 2 3 4 5 6 7 8 9 (α #'+ 5 5) (β #'+ #(1 2 3 4 1))))

(define-test indices-test
  (compute (array-indices #(5 6 7)))
  (let ((a (make-array '(2 3 4))))
    (compute (array-indices a 1))
    (compute (α #'+
                (array-indices a 0)
                (array-indices a 1)
                (array-indices a 2)))))


(define-test sum-of-pairs
  (let* ((size 10)
         (a (lazy-array (make-array size :initial-element 0))))
    (compute
     (β #'+ (fuse (reshape a (~ 0 (- size 1))
                           (τ (i) (0 i)))
                  (reshape a (~ 1 size)
                           (τ (i) (1 (1- i)))))))))

(define-test reduction-of-fusions
  (compute
   (β #'+ (fuse #(1 2 3)
                (reshape #(4 5 6) (τ (i) ((+ i 3))))))))
