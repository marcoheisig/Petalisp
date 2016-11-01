;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp type system

(defparameter petalisp-types
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)
    t))

(defun petalisp-type (type)
  "Given a Lisp type, return its corresponding petalisp type."
  (find type petalisp-types :test #'subtypep))

(defvar *operator-database* (make-hash-table :test #'equal))

(defun find-operator (function &rest types)
  (gethash (list* function types) *operator-database*))

#+nil
(defun make-operator (name &rest args &key domain-type lisp-function &allow-other-keys)
  (setf (gethash (list* (symbol-function name)
                        domain-type)
                 *operator-database*)
        (apply #'make-instance 'operator
               :name name
               :lisp-function (or lisp-function (symbol-function name))
               args)))

#+nil
(defmacro define-operator (name domain-type codomain-type &rest args)
  `(make-operator ',name
                  :domain-type ',domain-type
                  :codomain-type ',codomain-type ,@args))
