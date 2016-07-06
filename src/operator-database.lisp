;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Store and retrieve information on operators

(in-package :petalisp)

(defun petalispify-type (type)
  "Given a Lisp type, return its corresponding petalisp type."
  (upgraded-array-element-type type))

(defun petalisp-subtypep (type-1 type-2)
  "If TYPE-1 is a subtype of TYPE-2 (in the petalisp type system), returns
  true. Otherwise returns false."
  (subtypep type-1 type-2))

(defvar *operator-database* (make-hash-table :test #'eq))

(defun result-type (operator &rest types)
  "Returns the type of the result of applying OPERATOR to any arguments of
  types TYPES."
  (assert (functionp operator))
  (let ((op (gethash operator *operator-database*)))
    ;; TODO
    op))

(defun find-operator (operator) 'foo)
