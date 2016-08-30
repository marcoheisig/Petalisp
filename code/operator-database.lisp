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

(defmethod dimension ((object operator))
  (length (domain-type object)))

(defvar *operator-database* (make-hash-table :test #'equal))

(defun find-operator (function &rest types)
  (gethash (list* function types) *operator-database*))

(defun make-operator (name &rest args &key domain-type &allow-other-keys)
  (setf (gethash (list* (symbol-function name)
                        domain-type)
                 *operator-database*)
        (apply #'make-instance 'operator
               :name name
               args)))

(defmacro define-operator (name domain-type codomain-type &rest args)
  `(make-operator ',name
                  :domain-type ',domain-type
                  :codomain-type ',codomain-type ,@args))

(define-operator + (double-float double-float) (double-float)
  :cycles 1
  :loads 2
  :stores 1)

(define-operator * (double-float double-float) (double-float)
  :cycles 1
  :loads 2
  :stores 1)
