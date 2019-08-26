;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (/ div.integer) (rational) (integer integer))
(define-simple-instruction (/ div.rational) (rational) (rational rational))
(define-simple-instruction (/ div.short-float) (short-float) (short-float short-float))
(define-simple-instruction (/ div.single-float) (single-float) (single-float single-float))
(define-simple-instruction (/ div.double-float) (double-float) (double-float double-float))
(define-simple-instruction (/ div.long-float) (long-float) (long-float long-float))
(define-simple-instruction (/ div.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (/ div.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (/ div.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (/ div.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(define-instruction (/ div) (number) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (div.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (div.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (div.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (div.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (div.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (div.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (div.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (div.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (div.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (div.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t
     (rewrite-default
      (ntype 'number)))))

(define-rule / (number &rest more-numbers)
  (cond ((null more-numbers)
         (rewrite-as
          (div 1 number)))
        (t
         (reduce
          (lambda (a b) (rewrite-as (div a b)))
          more-numbers
          :initial-value number))))
