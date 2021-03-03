;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defmacro defalias (alias thing)
  (check-type alias symbol)
  `(ensure-alias ',alias ',thing))

(defun ensure-alias (alias designator)
  (cond ((and (symbolp designator)
              (special-operator-p designator))
         (error "Cannot aliases of special operators."))
        ((and (symbolp designator)
              (macro-function designator))
         (setf (macro-function alias)
               (macro-function designator))
         (setf (documentation alias 'function)
               (documentation designator 'function)))
        ((fboundp designator)
         (setf (fdefinition alias)
               (fdefinition designator))
         (setf (documentation alias 'function)
               (documentation designator 'function))
         (setf (compiler-macro-function alias)
               (compiler-macro-function designator))
         (setf (documentation alias 'compiler-macro)
               (documentation designator 'compiler-macro)))
        (t
         (error "Cannot create an alias of ~S."
                designator))))
