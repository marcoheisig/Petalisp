;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defmacro defalias (alias function)
  `(progn (setf (fdefinition ',alias) #',function)
          (setf (documentation ',alias 'function)
                (documentation ',function 'function))))

(defun free-variables (form &optional environment)
  (let (result)
    (agnostic-lizard:walk-form
     form environment
     :on-every-atom
     (lambda (form env)
       (prog1 form
         (when (and (symbolp form)
                    (not (find form (agnostic-lizard:metaenv-variable-like-entries env)
                               :key #'first)))
           (pushnew form result)))))
    result))

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
