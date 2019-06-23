;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (/ div.integer) (integer) (integer integer))
(defop (/ div.rational) (rational) (rational rational))
(defop (/ div.short-float) (short-float) (short-float short-float))
(defop (/ div.single-float) (single-float) (single-float single-float))
(defop (/ div.double-float) (double-float) (double-float double-float))
(defop (/ div.long-float) (long-float) (long-float long-float))
(defop (/ div.complex-short-float) (complex-short-float) (complex-short-float complex-short-float))
(defop (/ div.complex-single-float) (complex-single-float) (complex-single-float complex-single-float))
(defop (/ div.complex-double-float) (complex-double-float) (complex-double-float complex-double-float))
(defop (/ div.complex-long-float) (complex-long-float) (complex-long-float complex-long-float))
(defop (/ div) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
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
       (coerce-to-complex-long-float b))))))

(define-external-rewrite-rule / (&rest numbers)
  (if (null numbers)
      (rewrite-let () (rewrite-as 1))
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (rewrite-as (the-number number)))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (div a b)))))
        (values type-codes value))))
