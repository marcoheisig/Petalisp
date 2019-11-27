;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-specializer abs (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float (wrap (short-float-abs x)))
    (single-float (wrap (single-float-abs x)))
    (double-float (wrap (double-float-abs x)))
    (long-float (wrap (long-float-abs x)))
    (complex-short-float (wrap (complex-short-float-abs x)))
    (complex-single-float (wrap (complex-single-float-abs x)))
    (complex-double-float (wrap (complex-double-float-abs x)))
    (complex-long-float (wrap (complex-long-float-abs x)))
    (integer
     (wrap-default (ntype '(integer 0 *))))
    (real
     (wrap-default (ntype '(real 0 *))))
    (rational
     (wrap-default (ntype '(rational 0 *))))
    (t
     (wrap-default (ntype '(real 0 *))))))

(define-differentiator abs (x) _
  (wrap (if (< 0 x) 1 -1)))

(define-simple-instruction (abs short-float-abs) ((short-float 0S0 *)) (short-float))
(define-simple-instruction (abs single-float-abs) ((single-float 0F0 *)) (single-float))
(define-simple-instruction (abs double-float-abs) ((double-float 0D0 *)) (double-float))
(define-simple-instruction (abs long-float-abs) ((long-float 0L0 *)) (long-float))
(define-simple-instruction (abs complex-short-float-abs) ((short-float 0S0 *)) (complex-short-float))
(define-simple-instruction (abs complex-single-float-abs) ((single-float 0F0 *)) (complex-single-float))
(define-simple-instruction (abs complex-double-float-abs) ((double-float 0D0 *)) (complex-double-float))
(define-simple-instruction (abs complex-long-float-abs) ((long-float 0L0 *)) (complex-long-float))
