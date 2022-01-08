;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.multicore-backend
  (:use
   #:common-lisp
   #:petalisp.core)
  (:shadow
   #:make-request
   #:request
   #:requestp
   #:request-backend
   #:request-lazy-arrays
   #:request-wait)
  (:export
   #:make-multicore-backend))
