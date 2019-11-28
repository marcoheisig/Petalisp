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
    (unless (< -1 index (length wrappers))
      (error 'invalid-differentiation-index
             :function function
             :index index
             :arguments wrappers))
    (unless (ntype-subtypep (wrapper-ntype (nth index wrappers)) (ntype 'number))
      (error 'non-numeric-differentiation-argument
             :function function
             :index index
             :arguments wrappers))
    (apply (differentiator function) index wrappers)))
