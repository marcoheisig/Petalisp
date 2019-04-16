;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:defpackage :petalisp.type-codes
  (:use :common-lisp)
  (:export
   #:type-code
   #:type-code-complexp
   #:type-code-floatp
   #:type-code-signedp
   #:type-code-unsignedp
   #:type-code-bits

   #:type-code-from-type-specifier
   #:type-specifier-from-type-code
   #:type-code-of
   #:type-code-union

   #:define-type-code-cache
   #:access-type-code-cache))
