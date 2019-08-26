;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (+ add.integer) (integer) (integer integer))
(define-simple-instruction (+ add.rational) (rational) (rational rational))
(define-simple-instruction (+ add.short-float) (short-float) (short-float short-float))
(define-simple-instruction (+ add.single-float) (single-float) (single-float single-float))
(define-simple-instruction (+ add.double-float) (double-float) (double-float double-float))
(define-simple-instruction (+ add.long-float) (long-float) (long-float long-float))
(define-simple-instruction (+ add.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (+ add.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (+ add.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (+ add.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(define-instruction (+ add) (number) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (add.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (add.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (add.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (add.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (add.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (add.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (add.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (add.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (add.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (add.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t (rewrite-default number))))

(define-rule 1+ (number)
  (rewrite-as
   (add number 1)))

(define-rule + (&rest numbers)
  (trivia:match numbers
    ((list)
     (rewrite-as 0))
    ((list number)
     (rewrite-as
      (the-number number)))
    (_
     (reduce
      (lambda (a b) (rewrite-as (add a b)))
      numbers))))
