;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (cos cos.short-float) (short-float) (short-float))
(define-simple-instruction (cos cos.single-float) (single-float) (single-float))
(define-simple-instruction (cos cos.double-float) (double-float) (double-float))
(define-simple-instruction (cos cos.long-float) (long-float) (long-float))
(define-simple-instruction (cos cos.complex-short-float) (complex-short-float) (complex-short-float))
(define-simple-instruction (cos cos.complex-single-float) (complex-single-float) (complex-single-float))
(define-simple-instruction (cos cos.complex-double-float) (complex-double-float) (complex-double-float))
(define-simple-instruction (cos cos.complex-long-float) (complex-long-float) (complex-long-float))

(define-rule cos (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (cos.short-float x)))
    (single-float
     (rewrite-as
      (cos.single-float x)))
    (double-float
     (rewrite-as
      (cos.double-float x)))
    (long-float
     (rewrite-as
      (cos.long-float x)))
    (complex-short-float
     (rewrite-as
      (cos.complex-short-float x)))
    (complex-single-float
     (rewrite-as
      (cos.complex-single-float x)))
    (complex-double-float
     (rewrite-as
      (cos.complex-double-float x)))
    (complex-long-float
     (rewrite-as
      (cos.complex-long-float x)))
    (t
     (rewrite-default (ntype 'number)))))
