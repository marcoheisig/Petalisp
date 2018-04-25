;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/utilities/atomic-types
  (:use :closer-common-lisp :alexandria :trivia)
  (:export
   #:atomic-type
   #:complex-short-float
   #:complex-single-float
   #:complex-double-float
   #:complex-long-float
   . #.
   (loop for type in (append (loop for i from 1 to 64 collect `(signed-byte ,i))
                             (loop for i from 1 to 64 collect `(unsigned-byte ,i)))
         collect (make-symbol (format nil "~{~W~^-~}" type)))))

(in-package :petalisp/utilities/atomic-types)

;;; For some applications, it is desirable to use atomic type specifiers
;;; only. This library introduces atomic aliases for all upgraded array
;;; element types, and a function ATOMIC-TYPE to convert type specifiers to
;;; their atomic equivalent.

(deftype complex-short-float () '(complex short-float))
(deftype complex-single-float () '(complex single-float))
(deftype complex-double-float () '(complex double-float))
(deftype complex-long-float () '(complex long-float))

(defmacro define-atomic-integer-types ()
  (let* ((compound-types
           (append (loop for i from 1 to 64 collect `(signed-byte ,i))
                   (loop for i from 1 to 64 collect `(unsigned-byte ,i))))
         (alist
           (loop for compound-type in compound-types
                 for atomic-type = (format-symbol *package* "~{~W~^-~}" compound-type)
                 collect (cons compound-type atomic-type)))
         (signed-atomics
           (loop for (compound-type . atomic-type) in alist
                 when (eq (car compound-type) 'signed-byte)
                   collect atomic-type))
         (unsigned-atomics
           (loop for (compound-type . atomic-type) in alist
                 when (eq (car compound-type) 'unsigned-byte)
                   collect atomic-type)))
    `(progn
       ;; define all the atomic types
       ,@(loop for (compound-type . atomic-type) in alist
               collect `(deftype ,atomic-type () ',compound-type))
       (defun atomic-integer-type (signed-p bits)
         (declare (unsigned-byte bits))
         (if signed-p
             (if (> bits 64)
                 'signed-byte
                 (aref #(,@signed-atomics) (1- bits)))
             (if (> bits 64)
                 'unsigned-byte
                 (aref #(,@unsigned-atomics) (1- bits))))))))

(define-atomic-integer-types)

(defun atomic-type (type-specifier)
  "Return an atomic type specifier that is a supertype of TYPE."
  (match type-specifier
    ((type symbol) type-specifier)
    ((list 'signed-byte bits)
     (atomic-integer-type t bits))
    ((list 'unsigned-byte bits)
     (atomic-integer-type nil bits))
    ((list 'integer min max)
     (let ((bits (max (integer-length min)
                      (integer-length max))))
       (if (minusp min)
           (atomic-integer-type t bits)
           (atomic-integer-type nil bits))))
    ((list 'complex 'short-float) 'complex-short-float)
    ((list 'complex 'single-float) 'complex-single-float)
    ((list 'complex 'double-float) 'complex-double-float)
    ((list 'complex 'long-float) 'complex-long-float)
    (otherwise 't)))
