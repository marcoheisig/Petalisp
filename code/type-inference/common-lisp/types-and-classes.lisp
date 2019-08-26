;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-rule coerce (object result-type)
  (let ((object-ntype (wrapper-ntype object))
        (result-ntype (wrapper-ntype result-type)))
    (with-constant-folding (coerce (object-ntype t)
                                   (result-ntype type-specifier))
      (if (eql-ntype-p result-ntype)
          (let ((result-ntype (ntype result-ntype)))
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
               (rewrite-default result-ntype))))))))
