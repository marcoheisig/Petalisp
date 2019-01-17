;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-development)

(test linear-algebra-test
  (compute (dot #(1 2 3) #(4 5 6)))
  (compute (norm #(1 2 3)))
  (compute (argmax #(2 4 1 2 1)))
  (compute (nth-value 1 (argmax #(2 4 1 2 1))))
  (multiple-value-call #'compute (argmax #(2 4 1 2 1)))
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
