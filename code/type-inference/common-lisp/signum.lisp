;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-rule signum (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (signum (ntype number))
      (ntype-subtypecase ntype
        (rational (rewrite-default (ntype 'integer)))
        ((or float (complex float)) (rewrite-default ntype))
        (complex (rewrite-default (ntype 'complex)))
        (t (rewrite-default 'number))))))
