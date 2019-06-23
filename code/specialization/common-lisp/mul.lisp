;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (* mul.integer) (integer) (integer integer))
(defop (* mul.rational) (rational) (rational rational))
(defop (* mul.short-float) (short-float) (short-float short-float))
(defop (* mul.single-float) (single-float) (single-float single-float))
(defop (* mul.double-float) (double-float) (double-float double-float))
(defop (* mul.long-float) (long-float) (long-float long-float))
(defop (* mul.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(defop (* mul.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(defop (* mul.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(defop (* mul.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(defop (* mul) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
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
       (coerce-to-complex-long-float b))))))

(define-external-rewrite-rule * (&rest numbers)
  (if (null numbers)
      (rewrite-let () 1)
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (rewrite-as (the-number number)))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (mul a b)))))
        (values type-codes value))))

