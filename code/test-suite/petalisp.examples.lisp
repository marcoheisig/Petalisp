;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.examples.iterative-methods)
(check-package '#:petalisp.examples.linear-algebra)

(define-test jacobi-test
  (compute (jacobi (ndarray 1) 0.0 1.0 2))
  (compute (jacobi (ndarray 2) 0.0 1.0 2))
  (compute (jacobi (ndarray 3) 0.0 1.0 2))
  (compute (jacobi (ndarray 3) 0.0 1.0 5)))

(define-test rbgs-test
  (compute (rbgs (ndarray 1) 0.0 1.0 2))
  (compute (rbgs (ndarray 2) 0.0 1.0 2))
  (compute (rbgs (ndarray 3) 0.0 1.0 2))
  (compute (rbgs (ndarray 3) 0.0 1.0 5)))

(define-test v-cycle-test
  (compute (v-cycle (lazy-reshape 1.0 (~ 5 ~ 5)) 0.0 1.0 2 1))
  (compute (v-cycle (lazy-reshape 1.0 (~ 9 ~ 9)) 0.0 1.0 2 1))
  (compute (v-cycle (lazy-reshape 1.0 (~ 17 ~ 17)) 0.0 1.0 2 1))
  (compute (v-cycle (lazy-reshape 1.0 (~ 33 ~ 33)) 0.0 1.0 2 1))
  (compute (v-cycle (lazy-reshape 1.0 (~ 65 ~ 65)) 0.0 1.0 3 3)))

#+nil
(define-test iterate-randomly
  (flet ((act-randomly (array)
           (funcall
            (random-elt
             (list #'reshape-randomly
                   #'rbgs
                   #'jacobi))
                    array)))
    (loop repeat 10 do
      (let ((array (ndarray 3)))
        (loop repeat (random 8) do
          (setf array (act-randomly array)))
        (compute array)))))

(define-test linear-algebra-test
  (compute (dot #(1 2 3) #(4 5 6)))
  (compute (norm #(1 2 3)))
  (compute (max* #(2 4 1 2 1)))
  (compute (nth-value 1 (max* #(2 4 1 2 1))))
  (multiple-value-call #'compute (max* #(2 4 1 2 1)))
  (loop repeat 10 do
    (let* ((a (generate-matrix))
           (b (compute (transpose a))))
      (compute (matmul a b))))

  (let ((invertible-matrices
          '(#2A((42))
            #2A((1 1) (1 2))
            #2A((1 3 5) (2 4 7) (1 1 0))
            #2A((2 3 5) (6 10 17) (8 14 28))
            #2A((1 2 3) (4 5 6) (7 8 0))
            #2A(( 1 -1  1 -1  5)
                (-1  1 -1  4 -1)
                ( 1 -1  3 -1  1)
                (-1  2 -1  1 -1)
                ( 1 -1  1 -1  1)))))
    (loop for matrix in invertible-matrices do
      (multiple-value-bind (P L R) (lu matrix)
        (compute
         (matmul P (matmul L R)))))))
