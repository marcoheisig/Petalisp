;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (min min.integer) (integer) (integer integer))
(defop (min min.rational) (rational) (rational rational))
(defop (min min.short-float) (short-float) (short-float short-float))
(defop (min min.single-float) (single-float) (single-float single-float))
(defop (min min.double-float) (double-float) (double-float double-float))
(defop (min min.long-float) (long-float) (long-float long-float))
(defop (min min.real) (real) (real real) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not real) (abort-specialization))
    (integer
     (rewrite-as
      (min.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (min.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (min.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (min.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (min.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (min.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))))

(define-external-rewrite-rule min (real &rest more-reals)
  (if (null more-reals)
      (rewrite-let ((real (process-argument real)))
        (rewrite-as (the-real real)))
      (multiple-value-bind (min-type-codes min-value)
          (process-argument real)
        (loop until (null more-reals) do
          (multiple-value-setq (min-type-codes min-value)
            (rewrite-let ((a (values min-type-codes min-value))
                          (b (process-argument (pop more-reals))))
              (rewrite-as (min.real a b)))))
        (values min-type-codes min-value))))
