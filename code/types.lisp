;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp type system
;;;
;;; As of now, Petalisp only distinguishes the types DOUBLE-FLOAT,
;;; SINGLE-FLOAT, (COMPLEX DOUBLE-FLOAT), (COMPLEX SINGLE-FLOAT) and T.

(defparameter petalisp-types
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)
    t))

(defun petalisp-type (type)
  "Given a Lisp type, return its corresponding petalisp type."
   (find type petalisp-types :test #'subtypep))

(defvar *operator-database* (make-hash-table :test #'eq))

(defun result-type (operator &rest types)
  "Returns the type of the result of applying OPERATOR to any arguments of
  types TYPES."
  ;; TODO
  nil)

(defclass unary-operator (operator) ())

(defclass binary-operator (operator) ())

;; (define-operator + u64 u64 u64)


