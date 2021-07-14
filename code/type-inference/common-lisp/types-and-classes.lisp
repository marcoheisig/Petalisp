;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-specializer subtypep (type-1 type-2 &optional (environment nil environment-p))
  (with-constant-folding (subtypep ((wrapper-ntype type-1) type-specifier)
                                   ((wrapper-ntype type-2) type-specifier)
                                   ((if environment-p
                                        (wrapper-ntype environment)
                                        (ntype 'null))
                                    t))
    (wrap-default (ntype 'type-specifier))))

(define-specializer type-of (object)
  (wrap-default (ntype 'type-specifier)))

(define-specializer typep (object type-specifier &optional (environment nil environment-p))
  (let ((object-ntype (wrapper-ntype object))
        (type-specifier-ntype (wrapper-ntype type-specifier))
        (environment-ntype (if environment-p
                               (wrapper-ntype environment)
                               (ntype 'null))))
    (with-constant-folding (typep (object-ntype t)
                                  (type-specifier-ntype type-specifier)
                                  (environment-ntype t))
      (if (eql-ntype-p type-specifier-ntype)
          (let ((ntype (ntype type-specifier-ntype)))
            (cond ((ntype-subtypep object-ntype ntype)
                   (wrap-default (ntype '(not null))))
                  ((ntype-subtypepc2 object-ntype ntype)
                   (wrap nil))
                  (t (wrap-default (ntype 'generalized-boolean)))))
          (wrap-default (ntype 'generalized-boolean))))))
