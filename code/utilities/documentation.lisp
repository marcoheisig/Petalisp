;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun ensure-documentation (name new-value kind)
  (check-type new-value string)
  (symbol-macrolet ((documentation (documentation name kind)))
    (cond ((null documentation)
           (setf documentation new-value))
          ((string= documentation new-value)
           (values))
          (t
           (warn "Changing the ~(~A~) documentation of ~A." kind name)
           (setf documentation new-value)))))

(defmacro document-compiler-macro (name &body body)
  (assert (compiler-macro-function name))
  `(ensure-documentation ',name (progn ,@body) 'compiler-macro))

(defmacro document-function (name &body body)
  (assert (fboundp name))
  `(ensure-documentation ',name (progn ,@body) 'function))

(defmacro document-method-combination (name &body body)
  (assert (symbolp name))
  `(ensure-documentation ',name (progn ,@body) 'method-combination))

(defmacro document-setf-expander (name &body body)
  `(ensure-documentation ',name (progn ,@body) 'setf))

(defmacro document-structure (name &body body)
  (assert (typep (find-class name) 'structure-class))
  `(ensure-documentation ',name (progn ,@body) 'structure))

(defmacro document-type (name &body body)
  (typep nil name)
  `(ensure-documentation ',name (progn ,@body) 'type))

(defmacro document-variable (name &body body)
  `(ensure-documentation ',name (progn ,@body) 'variable))
