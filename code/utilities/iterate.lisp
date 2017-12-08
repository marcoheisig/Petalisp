;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(defmacro iterate (&body body)
  "ITERATE with type declarations enabled."
  `(iterate:iterate
     ,@(if (symbolp (first body))
           (list* (first body)
                  '(declare (iterate:declare-variables)) (rest body))
           (list* '(declare (iterate:declare-variables)) body))))
