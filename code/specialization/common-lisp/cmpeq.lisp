;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defop (= cmpeq.integer) (generalized-boolean) (integer integer))
(defop (= cmpeq.rational) (generalized-boolean) (rational rational))
(defop (= cmpeq.short-float) (generalized-boolean) (short-float short-float))
(defop (= cmpeq.single-float) (generalized-boolean) (single-float single-float))
(defop (= cmpeq.double-float) (generalized-boolean) (double-float double-float))
(defop (= cmpeq.long-float) (generalized-boolean) (long-float long-float))
(defop (= cmpeq.complex-short-float) (generalized-boolean) (complex-short-float complex-short-float))
(defop (= cmpeq.complex-single-float) (generalized-boolean) (complex-single-float complex-single-float))
(defop (= cmpeq.complex-double-float) (generalized-boolean) (complex-double-float complex-double-float))
(defop (= cmpeq.complex-long-float) (generalized-boolean) (complex-long-float complex-long-float))
(defop (= cmpeq) (generalized-boolean) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (cmpeq.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (cmpeq.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (cmpeq.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (cmpeq.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (cmpeq.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (cmpeq.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (cmpeq.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (cmpeq.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (cmpeq.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (cmpeq.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))))

(define-external-rewrite-rule = (number &rest more-numbers)
  (if (null more-numbers)
      (rewrite-let () (rewrite-as t))
      (multiple-value-bind (a-type-codes a-value)
          (process-argument number)
        (multiple-value-bind (result-type-codes result-value)
            (rewrite-let ((a (values a-type-codes a-value))
                          (b (process-argument (pop more-numbers))))
              (rewrite-as (cmpeq a b)))
          (loop while more-numbers do
            (multiple-value-setq (result-type-codes result-value)
              (rewrite-let ((result (values result-type-codes result-value))
                            (a (values a-type-codes a-value))
                            (b (process-argument (pop more-numbers))))
                (rewrite-as (and-fn result (cmpeq a b))))))
          (values result-type-codes result-value)))))
