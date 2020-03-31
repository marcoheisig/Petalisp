;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun parse-defmethod (args)
  (labels ((parse (qualifiers rest)
             (if (consp (car rest))
                 (values (reverse qualifiers) (car rest) (cdr rest))
                 (parse (cons (car rest) qualifiers) (cdr rest)))))
    (parse '() args)))

(defmacro define-method-pair (name &rest args)
  (multiple-value-bind (qualifiers lambda-list body)
      (parse-defmethod args)
    `(progn
       (defmethod ,name ,@qualifiers ,(identity lambda-list) ,@body)
       (defmethod ,name ,@qualifiers ,(reverse lambda-list) ,@body))))
