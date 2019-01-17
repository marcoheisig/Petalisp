;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

(define-type-inferrer identity (type)
  (values (list type) nil '() 'identity))
