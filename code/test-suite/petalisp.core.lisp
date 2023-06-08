;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(define-test range-test
  ;; Range constructors
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
             (is (range= range range))
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
                 (is (range= intersection1 intersection2))
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
;;; Array Element Types

(define-test array-element-test
  (loop for (element-type e1 e2)
          in '((single-float 1f0 2f0)
               (double-float 1d0 2d0)
               (bit 0 1)
               ((unsigned-byte 2) 1 2)
               ((unsigned-byte 4) 1 2)
               ((unsigned-byte 8) 1 2)
               ((unsigned-byte 16) 1 2)
               ((unsigned-byte 32) 1 2)
               ((unsigned-byte 64) 1 2)
               ((signed-byte 8) 1 2)
               ((signed-byte 16) 1 2)
               ((signed-byte 32) 1 2)
               ((signed-byte 64) 1 2)
               (fixnum 1 2)
               ((complex single-float) #.(complex 1f0 2f0) #.(complex 3f0 4f0))
               ((complex double-float) #.(complex 1d0 2d0) #.(complex 3d0 4d0))
               (function #'identity #'constantly)
               (symbol 'identity 'constantly)
               (cons '(1) '(2))
               (base-char #\a #\b)
               (character #\a #\b)
               (t 1 2))
        do (let ((a1 (make-array 100 :element-type element-type :initial-element e1))
                 (a2 (make-array 100 :element-type element-type :initial-element e2)))
             (compute
              (lazy-fuse
               (lazy-reshape a1 (~ 0 200 2))
               (lazy-reshape a2 (~ 0 200 2) (transform i to (1+ i)))))
             (compute
              (lazy-fuse
               (lazy-reshape e1 0 (~ 0 200 2))
               (lazy-reshape e2 0 (~ 1 200 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Networks and Automatic Differentiation

(define-test network-test
  (let* ((shape (~ 10))
         (x1 (make-unknown :shape shape :element-type 'double-float))
         (x2 (make-unknown :shape shape :element-type 'double-float))
         (v1 (lazy #'+
                   (lazy #'coerce (lazy #'log x1) 'double-float)
                   (lazy #'* x1 x2)
                   (lazy #'sin x2)))
         (network
           (make-network v1))
         (g1 (make-unknown
              :shape (lazy-array-shape v1)
              :element-type (lazy-array-element-type v1)))
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
   (lazy #'+ 2 3))
  (compute
   (lazy #'+ #(2 3 4) #(5 4 3)))
  (compute
   (lazy #'+ #2A((1 2) (3 4)) #2A((4 3) (2 1))))
  (compute
   (lazy #'floor #(1 2.5 1/2) 2)))

(define-test reduction-test
  (compute
   (lazy-reduce #'+ #(1 2 3)))
  (compute
   (lazy-reduce #'+ #2A((1 2 3) (6 5 4))))
  (compute
   (lazy-reduce
    (lambda (lmax lmin rmax rmin)
      (values (max lmax rmax) (min lmin rmin)))
    #(+1 -1 +2 -2 +3 -3)
    #(+1 -1 +2 -2 +3 -3)))
  (loop for n from 1 to 111 do
    (let* ((v (make-array n :initial-element 1 :element-type '(signed-byte 32)))
           (i (lazy-index-components v)))
      (multiple-value-bind (max imax min imin)
          (lazy-reduce
           (lambda (lmax limax lmin limin rmax rimax rmin rimin)
             (multiple-value-bind (max imax)
                 (if (> rmax lmax)
                     (values rmax rimax)
                     (values lmax limax))
               (multiple-value-bind (min imin)
                   (if (< rmin lmin)
                       (values rmin rimin)
                       (values lmin limin))
                 (values max imax min imin))))
           v i v i)
        (compute max imax min imin))))
  (compute
   (lazy-reduce (lambda (a b) (values a b)) #(3 2 1))
   (lazy-reduce (lambda (a b) (values b a)) #(3 2 1))))

(define-test fusion-test
  (compute
   (lazy-fuse (lazy-reshape (vector 4 5 6) (transform i to (+ i 3)))
              (vector 1 2 3)))
  (compute
   (lazy-overwrite (lazy-reshape 0.0 (~ 2 5 ~ 2 5))
                   (lazy-reshape 1.0 (~ 3 4 ~ 3 4)))))

(define-test reshape-test
  (compute (lazy-reshape 4 (~ 5)))
  (compute (lazy-reshape #(1 2 3) (transform i to (- i))) #(3 2 1))
  (compute (lazy-reshape #(1 2 3 4) (~ 1 3)))
  (compute (lazy-reshape (lazy-index-components (~ 1 10)) (~ 3 ~ 3)))
  (compute (lazy-reshape #2A((1 2) (3 4)) (transform i j to j i)))
  (compute (lazy-reshape #(1 2 3 4) (~ 1 3) (collapsing-reshaper) (~ 0 2 ~ 0 2)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'lazy-reshape (lazy-index-components (~ 1 101)) shapes)))
   (list (~ 0 5 ~ 0 5 ~ 0 4)
         (~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5)
         (~ 1 3 ~ 1 6 ~ 1 3 ~ 1 6)
         (~ 1 4 2 ~ 1 10 2 ~ 1 4 2 ~ 1 10 2)
         (~ 100)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'lazy-reshape (lazy-index-components (~ 1 201)) shapes)))
   (list (~ 0 2 ~ 0 5 ~ 0 5 ~ 0 4)
         (~ 0 2 ~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5)
         (~ 0 2 ~ 0 100)))
  (alexandria:map-permutations
   (lambda (shapes)
     (compute
      (apply #'lazy-reshape (lazy-index-components (~ 1 201)) shapes)))
   (list (~ 0 5 ~ 0 5 ~ 0 4 ~ 0 2)
         (~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5 ~ 0 2)
         (~ 0 100 ~ 0 2)))
  (compute
   (lazy-overwrite
    (lazy-reshape #2A((1 2 3) (4 5 6)) (transform i j to (+ 2 i) (+ 3 j)))
    (lazy-reshape 9 (transform to 3 4))))
  (signals error
    (compute (lazy-reshape #(1 2 3 4) (~ -1 4)))))

(define-test multiple-arguments
  (compute 1 2 3 4 5 6 7 8 9 (lazy #'+ 5 5) (lazy-reduce #'+ #(1 2 3 4 1))))

(define-test indices-test
  (compute (lazy-index-components #(5 6 7)))
  (let ((a (make-array '(2 3 4))))
    (compute (lazy-index-components a 1))
    (compute (lazy #'+
                (lazy-index-components a 0)
                (lazy-index-components a 1)
                (lazy-index-components a 2)))))


(define-test sum-of-pairs
  (let* ((size 10)
         (a (lazy-array (make-array size :initial-element 0))))
    (compute
     (lazy-reduce
      #'+
      (lazy-fuse (lazy-reshape a (~ 0 (- size 1))
                               (transform i to 0 i))
                 (lazy-reshape a (~ 1 size)
                               (transform i to 1 (1- i))))))))

(define-test reduction-of-fusions
  (compute
   (lazy-reduce
    #'+
    (lazy-fuse
     #(1 2 3)
     (lazy-reshape #(4 5 6) (transform i to (+ i 3)))))))
