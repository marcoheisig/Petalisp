;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (tan tan.short-float) (short-float) (short-float))
(define-simple-instruction (tan tan.single-float) (single-float) (single-float))
(define-simple-instruction (tan tan.double-float) (double-float) (double-float))
(define-simple-instruction (tan tan.long-float) (long-float) (long-float))
(define-simple-instruction (tan tan.complex-short-float) (complex-short-float) (complex-short-float))
(define-simple-instruction (tan tan.complex-single-float) (complex-single-float) (complex-single-float))
(define-simple-instruction (tan tan.complex-double-float) (complex-double-float) (complex-double-float))
(define-simple-instruction (tan tan.complex-long-float) (complex-long-float) (complex-long-float))

(define-rule tan (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (tan.short-float x)))
    (single-float
     (rewrite-as
      (tan.single-float x)))
    (double-float
     (rewrite-as
      (tan.double-float x)))
    (long-float
     (rewrite-as
      (tan.long-float x)))
    (complex-short-float
     (rewrite-as
      (tan.complex-short-float x)))
    (complex-single-float
     (rewrite-as
      (tan.complex-single-float x)))
    (complex-double-float
     (rewrite-as
      (tan.complex-double-float x)))
    (complex-long-float
     (rewrite-as
      (tan.complex-long-float x)))
    (t
     (rewrite-default (ntype 'number)))))
