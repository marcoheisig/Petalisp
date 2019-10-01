;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (abs short-float-abs) ((short-float 0S0 *)) (short-float))
(define-simple-instruction (abs single-float-abs) ((single-float 0F0 *)) (single-float))
(define-simple-instruction (abs double-float-abs) ((double-float 0D0 *)) (double-float))
(define-simple-instruction (abs long-float-abs) ((long-float 0L0 *)) (long-float))
(define-simple-instruction (abs complex-short-float-abs) ((short-float 0S0 *)) (complex-short-float))
(define-simple-instruction (abs complex-single-float-abs) ((single-float 0F0 *)) (complex-single-float))
(define-simple-instruction (abs complex-double-float-abs) ((double-float 0D0 *)) (complex-double-float))
(define-simple-instruction (abs complex-long-float-abs) ((long-float 0L0 *)) (complex-long-float))

(define-rule abs (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (short-float-abs x)))
    (single-float
     (rewrite-as
      (single-float-abs x)))
    (double-float
     (rewrite-as
      (double-float-abs x)))
    (long-float
     (rewrite-as
      (long-float-abs x)))
    (complex-short-float
     (rewrite-as
      (complex-short-float-abs x)))
    (complex-single-float
     (rewrite-as
      (complex-single-float-abs x)))
    (complex-double-float
     (rewrite-as
      (complex-double-float-abs x)))
    (complex-long-float
     (rewrite-as
      (complex-long-float-abs x)))
    (integer
     (rewrite-default (ntype '(integer 0 *))))
    (real
     (rewrite-default (ntype '(real 0 *))))
    (rational
     (rewrite-default (ntype '(rational 0 *))))
    (t
     (rewrite-default (ntype '(real 0 *))))))
