;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-evaluator local-evaluator
    (evaluate-kernels
     ((kernels (vector kernel)))
     (print kernels)))
