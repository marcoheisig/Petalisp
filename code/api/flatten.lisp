;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun flatten (array)
  (reshape array (~ 0 (1- (shape-size (shape array))))))
