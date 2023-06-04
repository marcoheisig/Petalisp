;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.native-backend
  (:use
   #:common-lisp
   #:petalisp.core
   #:petalisp.ir
   #:petalisp.codegen)
  (:shadow #:petalisp.core #:request #:backend)
  (:import-from #:petalisp.ir #:readers #:writers #:sources #:targets)
  (:export
   #:make-native-backend))
