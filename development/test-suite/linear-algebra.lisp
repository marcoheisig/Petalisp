;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

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
      (compute (matmul a b)))))
