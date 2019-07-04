;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (max max.integer) (integer) (integer integer))
(defop (max max.rational) (rational) (rational rational))
(defop (max max.short-float) (short-float) (short-float short-float))
(defop (max max.single-float) (single-float) (single-float single-float))
(defop (max max.double-float) (double-float) (double-float double-float))
(defop (max max.long-float) (long-float) (long-float long-float))
(defop (max max.real) (real) (real real) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not real) (abort-specialization))
    (integer
     (rewrite-as
      (max.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (max.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (max.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (max.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (max.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (max.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))))

(define-external-rewrite-rule max (real &rest more-reals)
  (if (null more-reals)
      (rewrite-let ((real (process-argument real)))
        (rewrite-as (the-real real)))
      (multiple-value-bind (max-type-codes max-value)
          (process-argument real)
        (loop until (null more-reals) do
          (multiple-value-setq (max-type-codes max-value)
            (rewrite-let ((a (values max-type-codes max-value))
                          (b (process-argument (pop more-reals))))
              (rewrite-as (max.real a b)))))
        (values max-type-codes max-value))))
