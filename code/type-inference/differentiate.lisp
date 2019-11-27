;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defun differentiate (function
                      wrappers
                      wrapper-ntype
                      wrap-constant
                      wrap-function
                      index)
  (let ((*wrapper-ntype* wrapper-ntype)
        (*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function))
    (apply (differentiator function) index wrappers)))
