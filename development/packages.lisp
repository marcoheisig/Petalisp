;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(cl:defpackage :petalisp-development
  (:nicknames :petalisp-dev)
  (:shadowing-import-from :petalisp-core #:set-difference #:set-equal)
  (:use
   :closer-common-lisp
   :alexandria
   :petalisp-core
   :petalisp-reference-backend
   :petalisp-ir-backend
   :petalisp-native-backend
   :petalisp-examples-iterative-methods
   :petalisp-examples-linear-algebra)
  (:export
   #:run-test-suite
   #:make-testing-backend
   #:view))

