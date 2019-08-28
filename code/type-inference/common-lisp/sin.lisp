;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (sin sin.short-float) (short-float) (short-float))
(define-simple-instruction (sin sin.single-float) (single-float) (single-float))
(define-simple-instruction (sin sin.double-float) (double-float) (double-float))
(define-simple-instruction (sin sin.long-float) (long-float) (long-float))
(define-simple-instruction (sin sin.complex-short-float) (complex-short-float) (complex-short-float))
(define-simple-instruction (sin sin.complex-single-float) (complex-single-float) (complex-single-float))
(define-simple-instruction (sin sin.complex-double-float) (complex-double-float) (complex-double-float))
(define-simple-instruction (sin sin.complex-long-float) (complex-long-float) (complex-long-float))

(define-rule sin (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (sin.short-float x)))
    (single-float
     (rewrite-as
      (sin.single-float x)))
    (double-float
     (rewrite-as
      (sin.double-float x)))
    (long-float
     (rewrite-as
      (sin.long-float x)))
    (complex-short-float
     (rewrite-as
      (sin.complex-short-float x)))
    (complex-single-float
     (rewrite-as
      (sin.complex-single-float x)))
    (complex-double-float
     (rewrite-as
      (sin.complex-double-float x)))
    (complex-long-float
     (rewrite-as
      (sin.complex-long-float x)))
    (t
     (rewrite-default (ntype 'number)))))
