;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun α (function &rest arrays)
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (single-value-lazy-map shape function inputs)))

(defun α* (n-values function &rest arrays)
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (multiple-value-lazy-map n-values shape function inputs)))
