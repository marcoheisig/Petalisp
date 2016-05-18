;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp operators

(in-package :petalisp)

(defclass petalisp-object () ())

(defclass operator (petalisp-object) ())

(defclass unary-operator (operator) ())

(defclass binary-operator (operator) ())

(defclass + (binary-operator) ())

(defclass - (binary-operator) ())

(defclass * (binary-operator) ())

(defclass / (binary-operator) ())
