;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(define-rewrite-rules identity (t) (object)
  (rewrite-as object))
