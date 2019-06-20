;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;; The constant vector +TYPES+ is the fundamental building block of this
;;; library.  It contains one entry for each performance-relevant Common
;;; Lisp type specifier.  The specifiers are sorted in most-specific-first
;;; order, so that a linear search with subtypep automatically yields the
;;; most specific entry.
(alexandria:define-constant +types+
    ;; We remove duplicate types here, mostly to get rid of superfluous
    ;; floating-point and complex types.
    (remove-duplicates
     #(nil
       character
       function
       null
       symbol
       cons
       list
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
     :test #'alexandria:type=)
  :test #'equalp)

(defconstant type-code-limit (length +types+))

(deftype type-code ()
  `(integer 0 (#. type-code-limit)))

(defun type-code-from-type-specifier (type-specifier)
  (the type-code (position type-specifier +types+ :test #'subtypep)))

(define-compiler-macro type-code-from-type-specifier (&whole form type-specifier)
  (if (constantp type-specifier)
      (locally (declare (notinline type-code-from-type-specifier))
        (type-code-from-type-specifier (eval type-specifier)))
      form))

(declaim (inline type-specifier-from-type-code))
(defun type-specifier-from-type-code (type-code)
  (declare (type-code type-code))
  (svref +types+ type-code))

(defun type-code-of (object)
  (macrolet ((type-code-of-dispatcher ()
               `(typecase object
                  ,@(loop for type across +types+
                          for type-code from 0
                          collect `(,type ,type-code)))))
    (type-code-of-dispatcher)))

(define-compiler-macro type-code-of (&whole form object)
  (if (constantp object)
      (locally (declare (notinline type-code-of))
        (type-code-of (eval object)))
      form))
