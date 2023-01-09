;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-multiple-value (function n-values &rest arrays)
  (lazy-multiple-value-map function n-values (lazy-broadcast-list-of-arrays arrays)))
