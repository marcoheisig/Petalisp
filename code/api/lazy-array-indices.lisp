;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-array-indices (array &optional (axis 0))
  (lazy-shape-indices
   (etypecase array
     (array (array-shape array))
     (lazy-array (lazy-array-shape array)))
   axis))
