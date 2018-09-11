;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

(in-suite petalisp)

(test linear-algebra
  (loop for dimension upto 2 do
    (compute
     (transpose (ndarray dimension))))
  (let ((a (ndarray 2))
        (b (ndarray 2)))
    (compute (matmul a b)))
  (compute (dot #(1 2 3) #(4 5 6)))
  (compute (norm #(1 2 3))))
