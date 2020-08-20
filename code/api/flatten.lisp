;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun flatten (array)
  (reshape array (~ 0 (shape-size (shape array)))))
