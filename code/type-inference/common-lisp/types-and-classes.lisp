;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-rule coerce (object type-specifier)
  (let ((object-ntype (wrapper-ntype object))
        (type-specifier-ntype (wrapper-ntype type-specifier)))
    (with-constant-folding (coerce (object-ntype t)
                                   (type-specifier-ntype type-specifier))
      (if (eql-ntype-p type-specifier-ntype)
          (let ((result-ntype (ntype type-specifier-ntype)))
            (if (ntype-subtypep object-ntype result-ntype)
                (rewrite-as object)
                (ntype-subtypecase result-ntype
                  (short-float
                   (rewrite-as
                    (coerce-to-short-float object)))
                  (single-float
                   (rewrite-as
                    (coerce-to-single-float object)))
                  (double-float
                   (rewrite-as
                    (coerce-to-double-float object)))
                  (long-float
                   (rewrite-as
                    (coerce-to-long-float object)))
                  (complex-short-float
                   (rewrite-as
                    (coerce-to-complex-short-float)))
                  (complex-single-float
                   (rewrite-as
                    (coerce-to-complex-single-float)))
                  (complex-double-float
                   (rewrite-as
                    (coerce-to-complex-double-float)))
                  (complex-long-float
                   (rewrite-as
                    (coerce-to-complex-long-float)))
                  (t
                   (rewrite-default result-ntype)))))
          (rewrite-default (ntype 't))))))

(define-rule subtypep (type-1 type-2 &optional (environment nil environment-p))
  (with-constant-folding (subtypep ((wrapper-ntype type-1) type-specifier)
                                   ((wrapper-ntype type-2) type-specifier)
                                   ((if environment-p
                                        (wrapper-ntype environment)
                                        (ntype 'null))
                                    t))
    (rewrite-default (ntype 'type-specifier))))

(define-rule type-of (object)
  (rewrite-default (ntype 'type-specifier)))

(define-rule typep (object type-specifier &optional (environment nil environment-p))
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
                   (rewrite-default (ntype '(not null))))
                  ((ntype-subtypepc1 object-ntype ntype)
                   (rewrite-as nil))
                  (t (rewrite-default (ntype 'generalized-boolean)))))
          (rewrite-default (ntype 'generalized-boolean))))))
