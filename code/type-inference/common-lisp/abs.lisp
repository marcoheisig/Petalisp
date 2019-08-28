;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (abs abs.short-float) ((short-float 0S0 *)) (short-float))
(define-simple-instruction (abs abs.single-float) ((single-float 0F0 *)) (single-float))
(define-simple-instruction (abs abs.double-float) ((double-float 0D0 *)) (double-float))
(define-simple-instruction (abs abs.long-float) ((long-float 0L0 *)) (long-float))
(define-simple-instruction (abs abs.complex-short-float) ((short-float 0S0 *)) (complex-short-float))
(define-simple-instruction (abs abs.complex-single-float) ((single-float 0F0 *)) (complex-single-float))
(define-simple-instruction (abs abs.complex-double-float) ((double-float 0D0 *)) (complex-double-float))
(define-simple-instruction (abs abs.complex-long-float) ((long-float 0L0 *)) (complex-long-float))

(define-rule abs (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (abs.short-float x)))
    (single-float
     (rewrite-as
      (abs.single-float x)))
    (double-float
     (rewrite-as
      (abs.double-float x)))
    (long-float
     (rewrite-as
      (abs.long-float x)))
    (complex-short-float
     (rewrite-as
      (abs.complex-short-float x)))
    (complex-single-float
     (rewrite-as
      (abs.complex-single-float x)))
    (complex-double-float
     (rewrite-as
      (abs.complex-double-float x)))
    (complex-long-float
     (rewrite-as
      (abs.complex-long-float x)))
    (integer
     (rewrite-default (ntype '(integer 0 *))))
    (real
     (rewrite-default (ntype '(real 0 *))))
    (rational
     (rewrite-default (ntype '(rational 0 *))))
    (t
     (rewrite-default (ntype '(real 0 *))))))
