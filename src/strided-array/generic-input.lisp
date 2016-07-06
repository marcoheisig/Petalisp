;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-constant (lisp-input) ()
  (:documentation
   "A Petalisp object that returns the same value for all indices."))

(defmethod generic-input ((object t) &rest arguments)
  (declare (ignore arguments))
  (make-instance
   'strided-array-constant
   :lisp-object object))
