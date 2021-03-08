;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-flatten (array)
  (lazy-reshape array (~ 0 (shape-size (array-shape array)))))
