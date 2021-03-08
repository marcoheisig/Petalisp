;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-collapse (array)
  (let* ((lazy-array (lazy-array array))
         (shape (lazy-array-shape lazy-array)))
    (lazy-reshape lazy-array (collapsing-transformation shape))))
