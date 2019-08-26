;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (- sub.integer) (integer) (integer integer))
(define-simple-instruction (- sub.rational) (rational) (rational rational))
(define-simple-instruction (- sub.short-float) (short-float) (short-float short-float))
(define-simple-instruction (- sub.single-float) (single-float) (single-float single-float))
(define-simple-instruction (- sub.double-float) (double-float) (double-float double-float))
(define-simple-instruction (- sub.long-float) (long-float) (long-float long-float))
(define-simple-instruction (- sub.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (- sub.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (- sub.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (- sub.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(define-instruction (- sub) (number) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (sub.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (sub.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (sub.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (sub.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (sub.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (sub.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (sub.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (sub.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (sub.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (sub.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t
     (rewrite-default
      (ntype 'number)))))

(define-simple-instruction (- neg.integer) (integer) (integer))
(define-simple-instruction (- neg.rational) (rational) (rational))
(define-simple-instruction (- neg.short-float) (short-float) (short-float))
(define-simple-instruction (- neg.single-float) (single-float) (single-float))
(define-simple-instruction (- neg.double-float) (double-float) (double-float))
(define-simple-instruction (- neg.long-float) (long-float) (long-float))
(define-simple-instruction (- neg.complex-short-float) (complex-short-float) (complex-short-float))
(define-simple-instruction (- neg.complex-single-float) (complex-single-float) (complex-single-float))
(define-simple-instruction (- neg.complex-double-float) (complex-double-float) (complex-double-float))
(define-simple-instruction (- neg.complex-long-float) (complex-long-float) (complex-long-float))
(define-instruction (- neg) (number) (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number) (abort-specialization))
    (integer (rewrite-as (neg.integer x)))
    (rational (rewrite-as (neg.rational x)))
    (short-float (rewrite-as (neg.short-float x)))
    (single-float (rewrite-as (neg.single-float x)))
    (double-float (rewrite-as (neg.double-float x)))
    (long-float (rewrite-as (neg.long-float x)))
    (complex-short-float (rewrite-as (neg.complex-short-float x)))
    (complex-single-float (rewrite-as (neg.complex-single-float x)))
    (complex-double-float (rewrite-as (neg.complex-double-float x)))
    (complex-long-float (rewrite-as (neg.complex-long-float x)))
    (t (rewrite-default (ntype 'number)))))

(define-rule 1- (number)
  (rewrite-as (sub number 1)))

(define-rule - (number &rest more-numbers)
  (cond ((null more-numbers)
         (rewrite-as (neg number)))
        (t
         (reduce
          (lambda (a b) (rewrite-as (sub a b)))
          more-numbers
          :initial-value number))))
