;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-array-interior (array &optional (width 1))
  (check-type width unsigned-byte)
  (let ((lazy-array (lazy-array array)))
    (lazy-reshape
     lazy-array
     (shape-interior (lazy-array-shape lazy-array) width))))
