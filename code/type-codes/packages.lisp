;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:defpackage :petalisp.type-codes
  (:use :common-lisp)
  (:export
   #:type-code
   #:type-code-limit
   #:type-specifier-from-type-code
   #:type-code-from-type-specifier
   #:type-code-of

   ;; Manipulation of type codes.
   #:with-type-code-caching
   #:type-code-union
   #:type-code-subtypecase

   ;; Type inference.
   #:define-type-inference-rule
   #:give-up-type-inference
   #:abort-type-inference
   #:values-type-codes))
