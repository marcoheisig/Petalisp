;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (sin short-float-sin) (short-float) (short-float))
(define-simple-instruction (sin single-float-sin) (single-float) (single-float))
(define-simple-instruction (sin double-float-sin) (double-float) (double-float))
(define-simple-instruction (sin long-float-sin) (long-float) (long-float))
(define-simple-instruction (sin complex-short-float-sin) (complex-short-float) (complex-short-float))
(define-simple-instruction (sin complex-single-float-sin) (complex-single-float) (complex-single-float))
(define-simple-instruction (sin complex-double-float-sin) (complex-double-float) (complex-double-float))
(define-simple-instruction (sin complex-long-float-sin) (complex-long-float) (complex-long-float))

(define-rule sin (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (short-float-sin x)))
    (single-float
     (rewrite-as
      (single-float-sin x)))
    (double-float
     (rewrite-as
      (double-float-sin x)))
    (long-float
     (rewrite-as
      (long-float-sin x)))
    (complex-short-float
     (rewrite-as
      (complex-short-float-sin x)))
    (complex-single-float
     (rewrite-as
      (complex-single-float-sin x)))
    (complex-double-float
     (rewrite-as
      (complex-double-float-sin x)))
    (complex-long-float
     (rewrite-as
      (complex-long-float-sin x)))
    (t
     (rewrite-default (ntype 'number)))))
