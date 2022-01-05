;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-flatten (array)
  (let ((lazy-array (lazy-array array)))
    (lazy-reshape lazy-array (~ 0 (shape-size (lazy-array-shape lazy-array))))))
