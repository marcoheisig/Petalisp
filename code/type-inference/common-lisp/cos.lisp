;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (cos short-float-cos) (short-float) (short-float))
(define-simple-instruction (cos single-float-cos) (single-float) (single-float))
(define-simple-instruction (cos double-float-cos) (double-float) (double-float))
(define-simple-instruction (cos long-float-cos) (long-float) (long-float))
(define-simple-instruction (cos complex-short-float-cos) (complex-short-float) (complex-short-float))
(define-simple-instruction (cos complex-single-float-cos) (complex-single-float) (complex-single-float))
(define-simple-instruction (cos complex-double-float-cos) (complex-double-float) (complex-double-float))
(define-simple-instruction (cos complex-long-float-cos) (complex-long-float) (complex-long-float))

(define-rule cos (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (rewrite-as
      (short-float-cos x)))
    (single-float
     (rewrite-as
      (single-float-cos x)))
    (double-float
     (rewrite-as
      (double-float-cos x)))
    (long-float
     (rewrite-as
      (long-float-cos x)))
    (complex-short-float
     (rewrite-as
      (complex-short-float-cos x)))
    (complex-single-float
     (rewrite-as
      (complex-single-float-cos x)))
    (complex-double-float
     (rewrite-as
      (complex-double-float-cos x)))
    (complex-long-float
     (rewrite-as
      (complex-long-float-cos x)))
    (t
     (rewrite-default (ntype 'number)))))
