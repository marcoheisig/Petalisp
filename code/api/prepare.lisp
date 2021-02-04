;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun prepare (&rest arrays)
  (values-list (prepare-list-of-arrays arrays)))

(defun prepare-list-of-arrays (list-of-arrays)
  (let ((lazy-arrays (mapcar #'lazy-array list-of-arrays)))
    (wait (apply #'schedule lazy-arrays))
    lazy-arrays))
