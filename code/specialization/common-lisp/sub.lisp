;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (- sub.integer) (integer) (integer integer))
(defop (- sub.rational) (rational) (rational rational))
(defop (- sub.short-float) (short-float) (short-float short-float))
(defop (- sub.single-float) (single-float) (single-float single-float))
(defop (- sub.double-float) (double-float) (double-float double-float))
(defop (- sub.long-float) (long-float) (long-float long-float))
(defop (- sub.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(defop (- sub.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(defop (- sub.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(defop (- sub.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(defop (- sub) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
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
       (coerce-to-complex-long-float b))))))

(defop (- neg.integer) (integer) (integer))
(defop (- neg.rational) (rational) (rational))
(defop (- neg.short-float) (short-float) (short-float))
(defop (- neg.single-float) (single-float) (single-float))
(defop (- neg.double-float) (double-float) (double-float))
(defop (- neg.long-float) (long-float) (long-float))
(defop (- neg.complex-short-float) (complex-short-float) (complex-short-float))
(defop (- neg.complex-single-float) (complex-single-float) (complex-single-float))
(defop (- neg.complex-double-float) (complex-double-float) (complex-double-float))
(defop (- neg.complex-long-float) (complex-long-float) (complex-long-float))
(defop (- neg) (number) (number) (x)
  (type-code-subtypecase x
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
    (complex-long-float (rewrite-as (neg.complex-long-float x)))))

(define-external-rewrite-rule - (&rest numbers)
  (if (null numbers)
      (abort-specialization)
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (if (null numbers)
                (rewrite-as (neg number))
                (rewrite-as (the-number number))))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (sub a b)))))
        (values type-codes value))))
