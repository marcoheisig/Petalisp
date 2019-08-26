;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (* mul.integer) (integer) (integer integer))
(define-simple-instruction (* mul.rational) (rational) (rational rational))
(define-simple-instruction (* mul.short-float) (short-float) (short-float short-float))
(define-simple-instruction (* mul.single-float) (single-float) (single-float single-float))
(define-simple-instruction (* mul.double-float) (double-float) (double-float double-float))
(define-simple-instruction (* mul.long-float) (long-float) (long-float long-float))
(define-simple-instruction (* mul.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (* mul.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (* mul.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (* mul.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(define-instruction (* mul) (number) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (mul.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (mul.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (mul.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (mul.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (mul.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (mul.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (mul.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (mul.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (mul.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (mul.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t
     (rewrite-default number))))

(define-rule * (&rest numbers)
  (trivia:match numbers
    ((list)
     (rewrite-as 0))
    ((list number)
     (rewrite-as
      (the-number number)))
    (_
     (reduce
      (lambda (a b) (rewrite-as (mul a b)))
      numbers))))

