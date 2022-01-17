;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.xmas-backend
  (:use
   #:common-lisp
   #:petalisp.core
   #:petalisp.ir)
  (:export
   #:make-xmas-backend))
