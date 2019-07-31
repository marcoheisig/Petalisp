;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defmacro defalias (alias function)
  `(progn
     (if (macro-function ',function)
         (setf (macro-function ',alias)
               (macro-function ',function))
         (setf (fdefinition ',alias)
               (fdefinition ',function)))
          (setf (documentation ',alias 'function)
                (documentation ',function 'function))))

(defalias a α)

(defalias b β)

(defalias b* β*)

(defalias alpha α)

(defalias beta β)

(defalias beta* β*)

(defalias tau τ)
