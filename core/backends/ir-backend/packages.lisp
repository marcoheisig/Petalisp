;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(cl:defpackage :petalisp-ir-backend
  (:use :closer-common-lisp :alexandria :petalisp-core)
  (:shadowing-import-from :petalisp-core #:set-difference #:set-equal)
  (:export
   #:make-ir-backend))

