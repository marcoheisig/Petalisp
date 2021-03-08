;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-multiple-value (function n-values &rest arrays)
  (multiple-value-bind (inputs shape)
      (lazy-broadcast-list-of-arrays arrays)
    (lazy-multiple-value-map function n-values shape inputs)))
