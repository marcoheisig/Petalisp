;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;; Ntypes - Non-consing types.
;;;
;;; An ntype is either an actual object of type NTYPE, or any other object.
;;; In the latter case, the object denotes the set of all objects that are
;;; EQL to it.  This convention has the unfortunate effect that objects of
;;; type NTYPE cannot be used as EQL specifiers themselves.  On the other
;;; hand, this convention makes ntypes much more useful and allows
;;; reasoning about forms such as (coerce x 'single-float) or (expt n 2).

(defstruct (ntype
            (:predicate %ntypep)
            (:copier nil)
            (:conc-name %ntype-)
            (:constructor %make-ntype (type-specifier id)))
  (type-specifier nil :read-only t)
  (id nil :type (unsigned-byte 8) :read-only t))

(defmethod print-object ((ntype ntype) stream)
  (print-unreadable-object (ntype stream :type t)
    (format stream "~S" (%ntype-type-specifier ntype))))

(defmethod make-load-form ((ntype ntype) &optional environment)
  (make-load-form-saving-slots ntype :environment environment))

(declaim (inline ntype=))
(defun ntype= (ntype-1 ntype-2)
  (= (%ntype-id ntype-1)
     (%ntype-id ntype-2)))

(defparameter *ntypes*
  (let ((type-specifiers
          ;; We remove duplicate types here, mostly to get rid of superfluous
          ;; floating-point and complex types.
          (remove-duplicates
           '(nil
             character
             function
             null
             (and symbol (not null))
             symbol
             cons
             list
             array
             short-float
             single-float
             double-float
             long-float
             float
             bit
             (unsigned-byte 2)
             (unsigned-byte 4)
             (unsigned-byte 8)
             (unsigned-byte 16)
             (unsigned-byte 32)
             (unsigned-byte 64)
             (signed-byte 8)
             (signed-byte 16)
             (signed-byte 32)
             (signed-byte 64)
             integer
             rational
             real
             (complex short-float)
             (complex single-float)
             (complex long-float)
             (complex double-float)
             complex
             number
             t)
           :test #'alexandria:type=)))
    (map 'simple-vector
         #'%make-ntype
         type-specifiers
         (alexandria:iota (length type-specifiers)))))

(defun type-specifier (ntype)
  (if (%ntypep ntype)
      (%ntype-type-specifier ntype)
      `(eql ,ntype)))

(defun ntype (type-specifier)
  (if (and (consp type-specifier)
           (eq (first type-specifier) 'eql)
           (consp (rest type-specifier))
           (null (rest (rest type-specifier)))
           (not (%ntypep (second type-specifier))))
      (second type-specifier)
      (the ntype
           (find type-specifier *ntypes*
                 :test #'subtypep
                 :key #'%ntype-type-specifier))))

(define-compiler-macro ntype (&whole form type-specifier)
  (if (constantp type-specifier)
      (locally (declare (notinline ntype))
        (ntype (eval type-specifier)))
      form))

;;; Explicitly check some invariants, just to make sure.

(loop for ntype across *ntypes*
      for id from 0 do
      (assert (= id (%ntype-id ntype))))
