;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.test-suite
  (:use
   #:common-lisp
   #:petalisp
   #:petalisp.examples.iterative-methods
   #:petalisp.examples.linear-algebra)
  (:export
   #:run-test-suite
   #:make-testing-backend
   #:define-test
   #:is
   #:signals
   #:all-tests
   #:check-package
   #:run-tests
   #:run-petalisp-test-suite))
