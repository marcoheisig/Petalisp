;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-array-indices (array &optional (axis 0))
  (lazy-shape-indices (array-shape array) axis))
