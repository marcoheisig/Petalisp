;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-rule plusp (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (plusp (ntype real))
      (ntype-subtypecase ntype
        ((real (0) *) (rewrite-default (ntype '(not null))))
        ((real * 0) (rewrite-as nil))
        (t (rewrite-default (ntype 'generalized-boolean)))))))
