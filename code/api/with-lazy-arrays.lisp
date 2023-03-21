;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defmacro with-lazy-arrays ((&rest names) &body body)
  `(let ,(mapcar
          (lambda (name)
            (etypecase name
              (symbol `(,name (lazy-array ,name)))
              ((cons symbol (cons t null))
               `(,(first name) (lazy-array ,(second name))))))
          names)
     ,@body))
