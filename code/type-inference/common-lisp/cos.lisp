;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-specializer cos (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (wrap
      (short-float-cos x)))
    (single-float
     (wrap
      (single-float-cos x)))
    (double-float
     (wrap
      (double-float-cos x)))
    (long-float
     (wrap
      (long-float-cos x)))
    (complex-short-float
     (wrap
      (complex-short-float-cos x)))
    (complex-single-float
     (wrap
      (complex-single-float-cos x)))
    (complex-double-float
     (wrap
      (complex-double-float-cos x)))
    (complex-long-float
     (wrap
      (complex-long-float-cos x)))
    (t
     (wrap-default (ntype 'number)))))

(define-differentiator cos (x) _
  (wrap (- (sin x))))

(define-simple-instruction (cos short-float-cos) (short-float) (short-float))
(define-simple-instruction (cos single-float-cos) (single-float) (single-float))
(define-simple-instruction (cos double-float-cos) (double-float) (double-float))
(define-simple-instruction (cos long-float-cos) (long-float) (long-float))
(define-simple-instruction (cos complex-short-float-cos) (complex-short-float) (complex-short-float))
(define-simple-instruction (cos complex-single-float-cos) (complex-single-float) (complex-single-float))
(define-simple-instruction (cos complex-double-float-cos) (complex-double-float) (complex-double-float))
(define-simple-instruction (cos complex-long-float-cos) (complex-long-float) (complex-long-float))
