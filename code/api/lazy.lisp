;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy (function &rest arrays)
  (multiple-value-bind (inputs shape)
      (lazy-broadcast-list-of-arrays arrays)
    (lazy-map function shape inputs)))
