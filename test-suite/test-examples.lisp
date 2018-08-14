;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

(in-suite petalisp)

(test jacobi
  (compute (jacobi (ndarray 1) :iterations 2))
  (compute (jacobi (ndarray 2) :iterations 2))
  (compute (jacobi (ndarray 3) :iterations 2))
  (compute (jacobi (ndarray 3) :iterations 5)))

(test red-black-gauss-seidel
  (compute (red-black-gauss-seidel (ndarray 1) :iterations 2))
  (compute (red-black-gauss-seidel (ndarray 2) :iterations 2))
  (compute (red-black-gauss-seidel (ndarray 3) :iterations 2))
  (compute (red-black-gauss-seidel (ndarray 3) :iterations 5)))

(test linear-algebra
  (loop for dimension upto 2 do
    (compute
     (transpose (ndarray dimension))))
  (let ((a (ndarray 2))
        (b (ndarray 2)))
    (compute (matmul a b)))
  (compute (dot #(1 2 3) #(4 5 6)))
  (compute (norm #(1 2 3))))
