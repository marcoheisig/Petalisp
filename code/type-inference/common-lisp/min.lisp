;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (min min.integer) (integer) (integer integer))
(define-simple-instruction (min min.rational) (rational) (rational rational))
(define-simple-instruction (min min.short-float) (short-float) (short-float short-float))
(define-simple-instruction (min min.single-float) (single-float) (single-float single-float))
(define-simple-instruction (min min.double-float) (double-float) (double-float double-float))
(define-simple-instruction (min min.long-float) (long-float) (long-float long-float))
(define-instruction (min min.real) (real) (a b)
  ;; TODO
  (rewrite-default real))

(define-rule min (real &rest more-reals)
  (cond ((null more-reals)
         (rewrite-as (the-real real)))
        (t
         (reduce
          (lambda (a b)
            (rewrite-as (min.real a b)))
          more-reals
          :initial-value real))))
