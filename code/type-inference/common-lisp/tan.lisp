;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (tan short-float-tan) (short-float) (short-float))
(define-simple-instruction (tan single-float-tan) (single-float) (single-float))
(define-simple-instruction (tan double-float-tan) (double-float) (double-float))
(define-simple-instruction (tan long-float-tan) (long-float) (long-float))
(define-simple-instruction (tan complex-short-float-tan) (complex-short-float) (complex-short-float))
(define-simple-instruction (tan complex-single-float-tan) (complex-single-float) (complex-single-float))
(define-simple-instruction (tan complex-double-float-tan) (complex-double-float) (complex-double-float))
(define-simple-instruction (tan complex-long-float-tan) (complex-long-float) (complex-long-float))

(define-specializer tan (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (wrap
      (short-float-tan x)))
    (single-float
     (wrap
      (single-float-tan x)))
    (double-float
     (wrap
      (double-float-tan x)))
    (long-float
     (wrap
      (long-float-tan x)))
    (complex-short-float
     (wrap
      (complex-short-float-tan x)))
    (complex-single-float
     (wrap
      (complex-single-float-tan x)))
    (complex-double-float
     (wrap
      (complex-double-float-tan x)))
    (complex-long-float
     (wrap
      (complex-long-float-tan x)))
    (t
     (wrap-default (ntype 'number)))))
