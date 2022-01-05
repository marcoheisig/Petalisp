;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;; FNDB - The Function Database

(defvar *fndb* (make-hash-table :test #'eq))

(defstruct (fndb-record
            (:copier nil)
            (:predicate fndb-record-p))
  (function-name nil :type function-name :read-only t)
  (parent nil)
  (specializer nil :type (or function null))
  (differentiator nil :type (or function null)))

(defun ensure-fndb-record (function-name)
  (declare (function-name function-name))
  (multiple-value-bind (record present-p)
      (gethash function-name *fndb*)
    (if present-p
        record
        (let ((record (make-fndb-record :function-name function-name)))
          (setf (gethash function-name *fndb*) record)
          (when (fboundp function-name)
            (unless (and (symbolp function-name)
                         (special-operator-p function-name))
              (setf (gethash (fdefinition function-name) *fndb*) record)))
          record))))

(define-compiler-macro ensure-fndb-record (&whole form function-name)
  (if (typep function-name '(cons (eql quote) (cons function-name null)))
      `(load-time-value
        (locally (declare (notinline ensure-fndb-record))
          (ensure-fndb-record ,function-name)))
      form))

(defmacro define-fndb-accessor (name accessor-name &optional (default nil))
  (let ((finder (intern (concatenate 'string (symbol-name '#:find-) (symbol-name name))
                        #.*package*)))
    `(progn
       ;; Define a writer.
       (defun (setf ,name) (value function)
         (setf (,accessor-name (ensure-fndb-record function)) value))
       ;; Define a lookup function on records instead of functions.  This
       ;; function makes use of a record's parent slot to provide a limited
       ;; form of inheritance.
       (defun ,finder (record)
         (cond
           ((null record) ,default)
           ((,accessor-name record))
           (t (,finder (fndb-record-parent record)))))
       ;; Define a reader.
       (defun ,name (function)
         (,finder (gethash function *fndb*)))
       ;; Define a compiler macro to move the hash table lookup to the load
       ;; time whenever FUNCTION is a constant function name.
       (define-compiler-macro ,name (&whole form function)
         (if (typep function '(cons (eql quote) (cons function-name null)))
             `(,',finder (ensure-fndb-record ,function))
             form)))))

(define-fndb-accessor parent fndb-record-parent)

(define-fndb-accessor specializer fndb-record-specializer
  (lambda (&rest arguments)
    (declare (ignore arguments))
    (give-up-specialization)))

(define-fndb-accessor differentiator fndb-record-differentiator
  (lambda (&rest arguments)
    (break "TODO")))
