;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.reference-backend
  (:use :common-lisp :alexandria :petalisp.core)
  (:shadowing-import-from :petalisp.core #:set-difference #:set-equal)
  (:export
   #:make-reference-backend))

