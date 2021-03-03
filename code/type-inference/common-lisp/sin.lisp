;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-specializer sin (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (wrap
      (short-float-sin x)))
    (single-float
     (wrap
      (single-float-sin x)))
    (double-float
     (wrap
      (double-float-sin x)))
    (long-float
     (wrap
      (long-float-sin x)))
    (complex-short-float
     (wrap
      (complex-short-float-sin x)))
    (complex-single-float
     (wrap
      (complex-single-float-sin x)))
    (complex-double-float
     (wrap
      (complex-double-float-sin x)))
    (complex-long-float
     (wrap
      (complex-long-float-sin x)))
    (t
     (wrap-default (ntype 'number)))))

(define-differentiator sin (x) _
  (wrap (cos x)))

(define-simple-instruction (sin short-float-sin) (short-float) (short-float))
(define-simple-instruction (sin single-float-sin) (single-float) (single-float))
(define-simple-instruction (sin double-float-sin) (double-float) (double-float))
(define-simple-instruction (sin long-float-sin) (long-float) (long-float))
(define-simple-instruction (sin complex-short-float-sin) (complex-short-float) (complex-short-float))
(define-simple-instruction (sin complex-single-float-sin) (complex-single-float) (complex-single-float))
(define-simple-instruction (sin complex-double-float-sin) (complex-double-float) (complex-double-float))
(define-simple-instruction (sin complex-long-float-sin) (complex-long-float) (complex-long-float))
