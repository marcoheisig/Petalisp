;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro define-instruction ((base-name instruction-name)
                              result-types arguments
                              &body body)
  `(progn
     (declaim (inline ,instruction-name))
     (defun ,instruction-name ,arguments
       (the (values ,@result-types)
            (,base-name ,@arguments)))
     (define-rule ,instruction-name ,arguments ,@body)))

(defmacro define-simple-instruction ((base-name instruction-name) result-types argument-types)
  (let ((arguments (mapcar #'gensymify argument-types)))
    `(define-instruction (,base-name ,instruction-name) ,result-types ,arguments
         ,@(loop for argument in arguments
                 for argument-type in argument-types
                 collect `(check-ntype ,argument ,argument-type))
         (rewrite-default ,@result-types))))

(defun gensymify (x)
  (if (symbolp x)
      (gensym (symbol-name x))
      (gensym)))
