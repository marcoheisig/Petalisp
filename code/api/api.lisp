;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defvar *backend* (make-native-backend))

(petalisp.utilities:defalias a α)

(petalisp.utilities:defalias b β)

(defmacro define-parallel-aliases (name)
  (let ((αsym (alexandria:symbolicate 'α name))
        (βsym (alexandria:symbolicate 'β name)))
    `(progn
       (declaim (inline ,αsym ,βsym))
       (defun ,αsym (&rest args)
         (apply #'α #',name args))
       (defun ,βsym (&rest args)
         (apply #'β #',name args))
       (define-compiler-macro ,αsym (&rest args)
         `(α #',name ,@args))
       (define-compiler-macro ,βsym (&rest args)
         `(β #',name ,@args))
       ',name)))
