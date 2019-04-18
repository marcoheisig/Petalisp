;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(define-type-inference-rule apply (function &rest args)
  (if (or (type-code-numberp function)
          (type-code-characterp function))
      +empty-type-code+
      +universal-type-code+))

 (define-type-inference-rule identity (type-code)
   type-code)

 (define-type-inference-rule values (&rest type-codes)
   (values-list type-codes))

 (define-type-inference-rule values-list (type-code)
   (values))

 (define-type-inference-rule constantly (type-code)
   +universal-type-code+)

(define-type-inference-rule fboundp ())
