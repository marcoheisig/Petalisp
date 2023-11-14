;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-multiple-value (n-values function &rest arrays)
  (lazy-multiple-value-map n-values function (broadcast arrays)))
