;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-core)

(defmacro define-class-predicate (class-name &key hyphenate)
  (check-type class-name symbol)
  (let ((predicate-name
          (if hyphenate
              (symbolicate class-name "-P")
              (symbolicate class-name "P"))))
    `(defgeneric ,predicate-name (object)
       (:method ((object t))
         (declare (ignore object))
         nil)
       (:method ((,class-name ,class-name))
         (declare (ignore ,class-name))
         t))))
