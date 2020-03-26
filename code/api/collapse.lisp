;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun collapse (array)
  (let ((lazy-array (lazy-array array)))
    (reshape lazy-array (collapsing-transformation (shape lazy-array)))))
