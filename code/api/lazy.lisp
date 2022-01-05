;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy (function &rest arrays)
  (lazy-map function (lazy-broadcast-list-of-arrays arrays)))
