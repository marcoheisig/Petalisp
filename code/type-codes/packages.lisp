;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.type-codes
  (:use
   #:common-lisp)
  (:export
   #:type-code
   #:type-code-limit
   #:type-specifier-from-type-code
   #:type-code-from-type-specifier
   #:type-code-of
   #:array-element-type-code
   #:empty-type-code-p

   ;; Manipulation of type codes.
   #:with-type-code-caching
   #:type-code-union
   #:type-code-subtypecase

   ;; Type inference.
   #:define-type-inference-rule
   #:define-predicate-type-inference-rule
   #:with-type-inference-barrier
   #:give-up-type-inference
   #:abort-type-inference
   #:values-type-codes))
