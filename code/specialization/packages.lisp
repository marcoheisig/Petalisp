;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.specialization
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
   #:subtypep-mask
   #:type-code-subtypecase
   #:type-code-subtypep

   ;; Specialization
   #:define-external-rewrite-rule
   #:define-rewrite-rules
   #:abort-specialization
   #:defop
   #:specialize
   #:specialize-verbosely))
