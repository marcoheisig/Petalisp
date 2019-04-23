;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(defun numeric-contagion (type-code-1 type-code-2)
  (declare (type-code type-code-1 type-code-2))
  (check-type-code type-code-1 number)
  (check-type-code type-code-2 number)
  (max type-code-1 type-code-2))

(defun binary-type-code+ (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    +universal-type-code+))

(define-type-inference-rule + (&rest type-codes)
  (if (null type-codes)
      (type-code-of 0)
      (reduce #'binary-type-code+ type-codes)))

 (define-type-inference-rule cl:float-sign (type-code-1 &optional (type-code-2 type-code-1))
   (if (and (type-code-floatp type-code-1)
            (type-code-floatp type-code-2))
       type-code-2
       +empty-type-code+))
