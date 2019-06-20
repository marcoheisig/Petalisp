;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(define-external-rewrite-rule apply () (function &rest args)
  (check-argument function function)
  (check-argument (car (last args)) list))

(define-rewrite-rules identity (t) (object)
  (rewrite-as object))
