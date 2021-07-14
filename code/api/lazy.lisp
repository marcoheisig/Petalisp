;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy (function &rest arrays)
  (lazy-map function (lazy-broadcast-list-of-arrays arrays)))
