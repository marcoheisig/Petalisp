;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.test-suite
  (:use
   #:common-lisp
   #:petalisp.core
   #:petalisp.api
   #:petalisp.reference-backend
   #:petalisp.ir-backend
   #:petalisp.native-backend
   #:petalisp.examples.iterative-methods
   #:petalisp.examples.linear-algebra)
  (:export
   #:run-test-suite
   #:make-testing-backend
   #:view))

