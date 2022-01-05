;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defun differentiate (function
                      wrappers
                      wrapper-ntype
                      wrap-constant
                      wrap-function
                      index)
  (check-type index argument-index)
  (let ((*wrapper-ntype* wrapper-ntype)
        (*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function))
    (unless (< -1 index (length wrappers))
      (error 'invalid-differentiation-index
             :function function
             :index index
             :arguments wrappers))
    (ntype-subtypecase (wrapper-ntype (nth index wrappers))
      ((not number)
       (error 'non-numeric-differentiation-argument
              :function function
              :index index
              :arguments wrappers))
      (t
       (apply (differentiator function) index wrappers)))))

(defun diff (function arguments n)
  (flet ((wrap-constant (object)
           (cons (ntype-of object) object))
         (wrap-function (ntypes function arguments)
           (let ((expression (cons function (mapcar #'cdr arguments))))
             (values-list
              (loop for ntype in ntypes
                    collect
                    (cons ntype expression))))))
    (let ((result
            (differentiate
             function
             arguments
             #'first
             #'wrap-constant
             #'wrap-function
             n)))
      (values (cdr result) (car result)))))
