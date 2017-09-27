;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-evaluator global-evaluator
    (evaluate-data-structures
     ((data-structures (vector data-structure)))
     (kernelize data-structures)))
