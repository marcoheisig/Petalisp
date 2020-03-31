;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.ir-backend
  (:use
   #:common-lisp
   #:petalisp.core
   #:petalisp.ir)
  (:export
   #:make-ir-backend))

