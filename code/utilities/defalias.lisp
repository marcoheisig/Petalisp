;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defmacro defalias (alias function)
  `(progn (setf (fdefinition ',alias) #',function)
          (setf (documentation ',alias 'function)
                (documentation ',function 'function))))
