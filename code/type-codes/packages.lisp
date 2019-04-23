;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:defpackage :petalisp.type-codes
  (:use :common-lisp)
  (:export
   #:type-code
   #:type-code-limit
   #:type-specifier-from-type-code
   #:type-code-from-type-specifier
   #:type-code-of
   #:with-type-code-caching
   #:type-code-union
   #:type-code-intersection
   #:type-code-numberp
   #:type-code-floatp
   #:type-code-complex-float-p
   #:type-code-integerp
   #:type-code-characterp
   #:define-type-inference-rule
   #:check-type-code
   #:values-type-codes))
