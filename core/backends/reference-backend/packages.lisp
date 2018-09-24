;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(cl:defpackage :petalisp-reference-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:make-reference-backend))

