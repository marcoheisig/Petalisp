;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (max max.integer) (integer) (integer integer))
(define-simple-instruction (max max.rational) (rational) (rational rational))
(define-simple-instruction (max max.short-float) (short-float) (short-float short-float))
(define-simple-instruction (max max.single-float) (single-float) (single-float single-float))
(define-simple-instruction (max max.double-float) (double-float) (double-float double-float))
(define-simple-instruction (max max.long-float) (long-float) (long-float long-float))
(define-instruction (max max.real) (real) (a b)
  ;; TODO
  (rewrite-default (ntype 'real)))

(define-rule max (real &rest more-reals)
  (cond ((null more-reals)
         (rewrite-as (the-real real)))
        (t
         (reduce
          (lambda (a b)
            (rewrite-as (max.real a b)))
          more-reals
          :initial-value real))))
