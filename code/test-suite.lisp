;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :petalisp-test-suite
  (:use :alexandria :cl :petalisp)
  (:import-from :fiveam
                #:*on-error* #:*on-failure*
                #:in-suite #:in-suite* #:test
                #:is #:is-true #:is-false #:signals #:for-all)
  (:shadowing-import-from :petalisp #:intersection))

(in-package :petalisp-test-suite)

;; "inherit" all symbols from the :petalisp package
#.(loop for symbol being the present-symbols of :petalisp
        with package = (find-package :petalisp-test-suite) do
          (ignore-errors
           (import symbol package)))

(in-suite* petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(test |(identical)|
  (is-true (identical '(1 1 1)))
  (is-true (identical #(1 1.0 1/1) :test #'=))
  (is-true (identical '(1 2.0 6/2 4d0) :key #'numberp :test #'eq))
  (is-true (identical "aaaAaa" :test #'char= :key #'char-upcase)))

(test scaled-permutation-matrix
  (is (scaled-permutation-matrix?
       (scaled-permutation-matrix 3 3 #(0 1 2) #(5 4 3))))
  (signals error
    (scaled-permutation-matrix 1 1 #(2) #(1)))
  (signals error
    (scaled-permutation-matrix 2 2 #(0 0) #(1 1))))

(test |(matrix-product scaled-permutation-matrix vector)|
  (is (equalp
       #(10 8)
       (matrix-product
        (scaled-permutation-matrix 2 2 #(1 0) #(2 2))
        #(4 5)))))

(test |(matrix-product scaled-permutation-matrix scaled-permutation-matrix)|
  (is (equalp
       (scaled-permutation-matrix 2 2 #(0 1) #(8 6))
       (matrix-product
        (scaled-permutation-matrix 2 2 #(1 0) #(2 2))
        (scaled-permutation-matrix 2 2 #(1 0) #(3 4)))))
  (is (equalp
       (scaled-permutation-matrix 3 5 #(0 0 0) #(0 0 0))
       (matrix-product
        (scaled-permutation-matrix 3 7 #(0 0 0) #(0 0 0))
        (scaled-permutation-matrix 7 5 #(0 1 2 3 4 0 0) #(6 6 6 6 6 0 0))))))

(test |(matrix-product scaled-permutation-matrix list)|
  (is (equalp
       '(0 foo 84 (* 3 foo))
       (matrix-product
        (scaled-permutation-matrix 4 4 #(0 1 2 3) #(0 1 2 3))
        '(foo foo 42 foo)))))

(test |(matrix-inverse scaled-permutation-matrix)|
  (is (equalp
       (scaled-permutation-matrix 4 4 #(2 0 1 3) #(2 3 4 5))
       (matrix-inverse
        (scaled-permutation-matrix 4 4 #(1 2 0 3) #(1/3 1/4 1/2 1/5))))))

(test |(identity-matrix? scaled-permutation-matrix)|
  (is-true  (identity-matrix? (scaled-permutation-matrix 3 3 #(0 1 2) #(1 1 1))))
  (is-false (identity-matrix? (scaled-permutation-matrix 3 3 #(0 1 2) #(1 2 1))))
  (is-false (identity-matrix? (scaled-permutation-matrix 3 3 #(1 0 2) #(1 1 1)))))

(test |(extended-euclid)|
  (flet ((? (u v)
           (multiple-value-bind (u1 u3) (extended-euclid u v)
             (is (= (gcd u v) u3))
             (if (zerop v)
                 (is (= u3 (* u u1)))
                 (is (integerp (/ (- u3 (* u u1)) v)))))))
    (? 0 0)
    (? 1 0)
    (? 0 1)
    (? (expt 6 40) (expt 9 40))
    (for-all ((u (integer-generator 0))
              (v (integer-generator 0)))
      (? u v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations

(test identity-transformation
  (let ((τ (τ (a b) (a b))))
    (is (identity-transformation? τ))
    (is (= 2 (input-dimension τ) (output-dimension τ))))
  (let ((τ (τ (a b c d) (a b (ash (* 2 c) -1) (+ d 0)))))
    (is (identity-transformation? τ))
    (is (= 4 (input-dimension τ) (output-dimension τ))))
  (for-all ((dimension (integer-generator 0 200)))
    (let ((τ (identity-transformation dimension)))
      (is (identity-transformation? τ))
      (is (equal? τ τ))
      (is (equal? τ (inverse τ)))
      (is (equal? τ (composition τ τ))))))

(test affine-transformation
  (dolist (τ (list (τ (m n) (n m))
                   (τ (m) ((* 2 m)))
                   (τ (m) ((/ (+ (* 90 (+ 2 m)) 15) 2)))
                   (τ (m) (m 1 2 3))
                   (τ (m) (5 9 m 2))
                   (τ (0 n 0) (n))
                   (τ (i j 5) (i j))))
    (let ((τ-inv (inverse τ)))
      (is (affine-transformation? τ))
      (is (equal? τ (inverse τ-inv)))
      (when (and (every #'null (input-constraints τ))
                 (every #'null (input-constraints τ-inv)))
        (is (equal? (composition τ-inv τ)
                    (identity-transformation
                     (input-dimension τ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(test |(range-generator)|
  (let ((max-size 10)
        (max-extent 100))
    (for-all ((range (range-generator :max-extent max-extent
                                      :max-size max-size)))
      (is (<= (abs (range-start range)) max-extent))
      (is (<= (abs (range-end range)) max-extent))
      (is (<= (size range) max-size)))))

(test |(difference range)|
  (for-all ((a (range-generator :max-extent 100)))
    (for-all ((b (range-generator :max-extent 100
                                  :intersecting a)))
      (is (equal? a (apply #'fusion
                           (intersection a b)
                           (difference a b)))))))

(test |(intersection range)|
  (let ((fiveam::*num-trials* (ceiling (sqrt fiveam::*num-trials*))))
    (for-all ((a (range-generator :max-extent 10000)))
      (for-all ((b (range-generator :max-extent 10000
                                    :intersecting a)))
        (let ((intersection (intersection a b)))
          (is-true (subspace? intersection a))
          (is-true (subspace? intersection b))
          (is (not (difference intersection a)))
          (is (not (difference intersection b))))))))

(test range
  (is (range? (range 0 0 0)))
  (signals error (range 0 0 1))
  (for-all ((start (integer-generator))
            (step (integer-generator 1))
            (end (integer-generator)))
    (is (range? (range start step end)))
    (is (= (size (range start step end))
           (1+ (floor (abs (- start end)) (abs step)))))
    (is (equal? (range start step end)
                (range start (- step) end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strided Array Index Spaces

(test |(difference strided-array-index-space)|
  (let ((fiveam::*num-trials* (ceiling (sqrt fiveam::*num-trials*))))
    (for-all ((a (strided-array-index-space-generator :dimension 3
                                                      :max-extent 40)))
      (for-all ((b (strided-array-index-space-generator :dimension 3
                                                        :intersecting a
                                                        :max-extent 40)))
        (is (equal? a (apply #'fusion
                             (intersection a b)
                             (difference a b))))))))

(test |(intersection strided-array-index-space)|
  (flet ((? (a b result)
           (is (equal? result (intersection a b)))))
    (?  (σ) (σ) (σ))
    (? (σ (0 9) (0 9)) (σ (2 10) (2 10)) (σ (2 9) (2 9)))
    (? (σ (1 2 3) (0 3 6)) (σ (1 1 3) (0 2 6)) (σ (1 2 3) (0 6 6)))))

(test |(generic-unery-funcall affine-transformation strided-array-index-space)|
  (flet ((? (object transformation result)
           (is (equal? result (funcall transformation object)))
           (is (equal? object (funcall (inverse transformation) result)))))
    (? (σ (1 1 1)) (τ (m) ((1+ m)))
       (σ (2 1 2)))
    (? (σ (0 9) (0 5)) (τ (m n) (n m))
       (σ (0 5) (0 9)))
    (? (σ (1 1) (2 2) (3 3)) (τ (1 2 3) (4))
       (σ (4 4)))
    (? (σ (2 2)) (τ (m) (1 m 3))
       (σ (1 1) (2 2) (3 3)))
    (? (σ (0 5 10) (0 7 21))
       (τ (m n)
          ((+ (* 2 n) 5)
           (+ (* 3 m) 99)))
       (σ (5 14 47) (99 15 129)))
    (? (σ (0 0) (0 0) (0 0) (0 0) (0 0))
       (τ (a 0 c 0 e) (0 a 0 c 0 e 0))
       (σ (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)))
    (signals error
      (funcall (τ (1 m) (m 1)) (σ (0 0) (0 0))))))

(test |(subdivide strided-array-index-space)|
  (flet ((? (&rest args)
           (let ((result (subdivision args)))
             ;; check for disjointness
             (let (intersections)
               (map-combinations
                (lambda (x)
                  (push (apply #'intersection x) intersections))
                result :length 2)
               (is (every #'null intersections)))
             ;; check for coverage
             (let ((fusion (apply #'fusion result)))
               (is (every (λ x (subspace? x fusion)) args))))))
    (? (σ (1 1 4)) (σ (1 2 5)))
    (? (σ (1 1 10) (1 1 10))
       (σ (5 1 10) (5 1 10)))))

(test |(subspace? strided-array-index-space)|
  (is (subspace? (range 1 1 2) (range 0 1 3)))
  (is (subspace? (range 0 4 8) (range 0 2 10)))
  (is (subspace? (σ (0 6 120) (1 1 100))
                 (σ (0 2 130) (0 1 101)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Whole Programs

(test petalisp-api
  (let ((*virtual-machine*
          (make-instance 'testing-virtual-machine
            :virtual-machines
            (list
             (make-instance 'reference-virtual-machine)
             (make-instance 'common-lisp-virtual-machine)))))
    (flet ((check (&rest recipes)
             (is-true
              (apply #'compute recipes))))
      (check (α #'+ 2 3))
      (check (α #'+ #(2 3 4) #(5 4 3)))
      (check (-> #(1 2 3) (τ (i) ((- i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test Suite Interface

(defun run-test-suite ()
  (format t "~&== Testing Petalisp ==~%")
  (print-platform-information)
  (print-system-statistics :petalisp)
  (print-package-statistics :petalisp)
  (fiveam:run! 'petalisp))
