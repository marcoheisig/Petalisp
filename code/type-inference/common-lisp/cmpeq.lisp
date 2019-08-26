;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (= cmpeq.integer) (generalized-boolean) (integer integer))
(define-simple-instruction (= cmpeq.rational) (generalized-boolean) (rational rational))
(define-simple-instruction (= cmpeq.short-float) (generalized-boolean) (short-float short-float))
(define-simple-instruction (= cmpeq.single-float) (generalized-boolean) (single-float single-float))
(define-simple-instruction (= cmpeq.double-float) (generalized-boolean) (double-float double-float))
(define-simple-instruction (= cmpeq.long-float) (generalized-boolean) (long-float long-float))
(define-simple-instruction (= cmpeq.complex-short-float) (generalized-boolean) (complex-short-float complex-short-float))
(define-simple-instruction (= cmpeq.complex-single-float) (generalized-boolean) (complex-single-float complex-single-float))
(define-simple-instruction (= cmpeq.complex-double-float) (generalized-boolean) (complex-double-float complex-double-float))
(define-simple-instruction (= cmpeq.complex-long-float) (generalized-boolean) (complex-long-float complex-long-float))
(define-instruction (= cmpeq) (generalized-boolean) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
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
       (coerce-to-complex-long-float b))))
    (t
     (rewrite-default
      (ntype 'generalized-boolean)))))

(define-rule = (number &rest more-numbers)
  (if (null more-numbers)
      (rewrite-as
       (prog2-fn
        (the-number number)
        t))
      (apply
       (find-rule 'and)
       (mapcar
        (lambda (other-number)
          (rewrite-as (cmpeq number other-number)))
        more-numbers))))
