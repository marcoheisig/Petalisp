;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(declaim (inline vectorize))
(defun vectorize (function &optional (arity 1))
  (lambda (&rest args)
    (apply #'α* arity function args)))
