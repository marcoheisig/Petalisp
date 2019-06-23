;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (/= cmpneq.integer) (generalized-boolean) (integer integer))
(defop (/= cmpneq.rational) (generalized-boolean) (rational rational))
(defop (/= cmpneq.short-float) (generalized-boolean) (short-float short-float))
(defop (/= cmpneq.single-float) (generalized-boolean) (single-float single-float))
(defop (/= cmpneq.double-float) (generalized-boolean) (double-float double-float))
(defop (/= cmpneq.long-float) (generalized-boolean) (long-float long-float))
(defop (/= cmpneq.complex-short-float) (generalized-boolean) (complex-short-float complex-short-float))
(defop (/= cmpneq.complex-single-float) (generalized-boolean) (complex-single-float complex-single-float))
(defop (/= cmpneq.complex-double-float) (generalized-boolean) (complex-double-float complex-double-float))
(defop (/= cmpneq.complex-long-float) (generalized-boolean) (complex-long-float complex-long-float))
(defop (= cmpneq) (generalized-boolean) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (cmpneq.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (cmpneq.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (cmpneq.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (cmpneq.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (cmpneq.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (cmpneq.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (cmpneq.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (cmpneq.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (cmpneq.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (cmpneq.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))))
