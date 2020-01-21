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
  (declare (ignore environment))
  `(svref *ntypes* ,(%ntype-id ntype)))

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
             complex-short-float
             complex-single-float
             complex-double-float
             complex-long-float
             complex
             number
             (not null)
             t)
           :test #'alexandria:type=
           :from-end t)))
    (map 'simple-vector
         #'%make-ntype
         type-specifiers
         (alexandria:iota (length type-specifiers)))))

;;; Explicitly check some invariants, just to make sure.

(loop for ntype across *ntypes*
      for id from 0 do
      (assert (= id (%ntype-id ntype))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Specifier Ntypes

(defun %ntype (type-specifier)
  ;; Check that we are dealing with a valid type specifier.
  (unless (ignore-errors (prog1 t (typep 42 type-specifier)))
    (error "~@<~S is not a valid type specifier.~:@>" type-specifier))
  (if (and (consp type-specifier)
           (eq (first type-specifier) 'eql)
           (consp (rest type-specifier))
           (null (rest (rest type-specifier)))
           (not (%ntypep (second type-specifier))))
      (second type-specifier)
      (find type-specifier *ntypes*
            :test #'subtypep
            :key #'%ntype-type-specifier)))

(defparameter *ntype-cache*
  (let ((table (make-hash-table :test #'equal)))
    ;; Register all known ntypes.
    (loop for ntype across *ntypes* do
      (setf (gethash (%ntype-type-specifier ntype) table)
            ;; We treat NULL specially, because it is a singleton type.
            (if (eql (%ntype-type-specifier ntype) 'null)
                nil
                ntype)))
    ;; Also register a few other commonly used types.
    (dolist (type-specifier '((complex short-float)
                              (complex single-float)
                              (complex double-float)
                              (complex long-float)))
      (setf (gethash type-specifier table)
            (%ntype type-specifier)))
    table))

(defun ntype (type-specifier)
  ;; Searching via subtypep is very slow, so we try to avoid it in
  ;; the common cases.
  (multiple-value-bind (ntype present-p)
      (gethash type-specifier *ntype-cache*)
    (if present-p
        ntype
        (let ((ntype (%ntype type-specifier)))
          ;; After looking up an atomic type specifier, place it in
          ;; the *ntype-cache* for future use.
          (when (symbolp type-specifier)
            (setf (gethash type-specifier *ntype-cache*) ntype))
          ntype))))

(define-compiler-macro ntype (&whole form type-specifier)
  (if (constantp type-specifier)
      (locally (declare (notinline ntype))
        (ntype (eval type-specifier)))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ntype Type Specifiers

(defun type-specifier (ntype)
  (if (%ntypep ntype)
      (%ntype-type-specifier ntype)
      `(eql ,ntype)))
