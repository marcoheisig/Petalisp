;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy (function &rest arrays)
  (lazy-map 1 function (broadcast arrays)))
