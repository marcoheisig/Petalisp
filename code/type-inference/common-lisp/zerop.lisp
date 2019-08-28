;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-rule zerop (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (zerop (ntype number))
      (ntype-subtypecase ntype
        (zero (rewrite-default (ntype '(not null))))
        ((not zero) (rewrite-as nil))
        (t (rewrite-default (ntype 'generalized-boolean)))))))
