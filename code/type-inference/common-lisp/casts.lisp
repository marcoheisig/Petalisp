;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-Point Casts

(declaim (inline coerce-to-short-float))
(defun coerce-to-short-float (number)
  (coerce number 'short-float))

(define-simple-instruction (coerce-to-short-float short-float-from-single-float) (short-float) (single-float))
(define-simple-instruction (coerce-to-short-float short-float-from-double-float) (short-float) (double-float))
(define-simple-instruction (coerce-to-short-float short-float-from-long-float) (short-float) (long-float))

(define-rule coerce-to-short-float (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (coerce-to-short-float (ntype real))
      (ntype-subtypecase ntype
        (short-float
         (rewrite-as real))
        (single-float
         (rewrite-as
          (short-float-from-single-float real)))
        (double-float
         (rewrite-as
          (short-float-from-double-float real)))
        (long-float
         (rewrite-as
          (short-float-from-long-float real)))
        (t
         (rewrite-default (ntype 'short-float)))))))

(declaim (inline coerce-to-single-float))
(defun coerce-to-single-float (number)
  (coerce number 'single-float))

(define-simple-instruction (coerce-to-single-float single-float-from-short-float) (single-float) (short-float))
(define-simple-instruction (coerce-to-single-float single-float-from-double-float) (single-float) (double-float))
(define-simple-instruction (coerce-to-single-float single-float-from-long-float) (single-float) (long-float))

(define-rule coerce-to-single-float (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (coerce-to-single-float (ntype real))
      (ntype-subtypecase ntype
        (single-float
         (rewrite-as real))
        (short-float
         (rewrite-as
          (single-float-from-short-float real)))
        (double-float
         (rewrite-as
          (single-float-from-double-float real)))
        (long-float
         (rewrite-as
          (single-float-from-long-float real)))
        (t
         (rewrite-default (ntype 'single-float)))))))

(declaim (inline coerce-to-double-float))
(defun coerce-to-double-float (number)
  (coerce number 'double-float))

(define-simple-instruction (coerce-to-double-float double-float-from-short-float) (double-float) (short-float))
(define-simple-instruction (coerce-to-double-float double-float-from-single-float) (double-float) (single-float))
(define-simple-instruction (coerce-to-double-float double-float-from-long-float) (double-float) (long-float))

(define-rule coerce-to-double-float (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (coerce-to-double-float (ntype real))
      (ntype-subtypecase ntype
        (double-float
         (rewrite-as real))
        (short-float
         (rewrite-as
          (double-float-from-short-float real)))
        (single-float
         (rewrite-as
          (double-float-from-single-float real)))
        (long-float
         (rewrite-as
          (double-float-from-long-float real)))
        (t
         (rewrite-default (ntype 'double-float)))))))

(declaim (inline coerce-to-long-float))
(defun coerce-to-long-float (number)
  (coerce number 'long-float))

(define-simple-instruction (coerce-to-long-float long-float-from-short-float) (long-float) (short-float))
(define-simple-instruction (coerce-to-long-float long-float-from-single-float) (long-float) (single-float))
(define-simple-instruction (coerce-to-long-float long-float-from-double-float) (long-float) (double-float))

(define-rule coerce-to-long-float (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (coerce-to-long-float (ntype real))
      (ntype-subtypecase ntype
        (long-float
         (rewrite-as real))
        (short-float
         (rewrite-as
          (long-float-from-short-float real)))
        (single-float
         (rewrite-as
          (long-float-from-single-float real)))
        (double-float
         (rewrite-as
          (long-float-from-double-float real)))
        (t
         (rewrite-default (ntype 'long-float)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Complex Floating-Point Casts

(declaim (inline coerce-to-complex-short-float))
(defun coerce-to-complex-short-float (number)
  (coerce number 'complex-short-float))

(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-short-float) (complex-short-float) (short-float))
(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-complex-single-float) (complex-short-float) (complex-single-float))
(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-complex-double-float) (complex-short-float) (complex-double-float))
(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-complex-long-float) (complex-short-float) (complex-long-float))

(define-rule coerce-to-complex-short-float (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (coerce-to-complex-short-float (ntype number))
      (ntype-subtypecase ntype
        (short-float
         (rewrite-as
          (complex-short-float-from-short-float number)))
        (single-float
         (rewrite-as
          (complex-short-float-from-short-float
           (short-float-from-single-float number))))
        (double-float
         (rewrite-as
          (complex-short-float-from-short-float
           (short-float-from-double-float number))))
        (long-float
         (rewrite-as
          (complex-short-float-from-short-float
           (short-float-from-long-float number))))
        (complex-short-float
         (rewrite-as number))
        (complex-single-float
         (rewrite-as
          (complex-short-float-from-complex-single-float number)))
        (complex-double-float
         (rewrite-as
          (complex-short-float-from-complex-double-float number)))
        (complex-long-float
         (rewrite-as
          (complex-short-float-from-complex-long-float number)))
        (t
         (rewrite-default (ntype 'complex-short-float)))))))

(declaim (inline coerce-to-complex-single-float))
(defun coerce-to-complex-single-float (number)
  (coerce number 'complex-single-float))

(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-single-float) (complex-single-float) (single-float))
(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-complex-short-float) (complex-single-float) (complex-short-float))
(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-complex-double-float) (complex-single-float) (complex-double-float))
(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-complex-long-float) (complex-single-float) (complex-long-float))

(define-rule coerce-to-complex-single-float (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (coerce-to-complex-single-float (ntype number))
      (ntype-subtypecase ntype
        (short-float
         (rewrite-as
          (complex-single-float-from-single-float
           (single-float-from-short-float number))))
        (single-float
         (rewrite-as
          (complex-single-float-from-single-float number)))
        (double-float
         (rewrite-as
          (complex-single-float-from-single-float
           (single-float-from-double-float number))))
        (long-float
         (rewrite-as
          (complex-single-float-from-single-float
           (single-float-from-long-float number))))
        (complex-single-float
         (rewrite-as number))
        (complex-short-float
         (rewrite-as
          (complex-single-float-from-complex-short-float number)))
        (complex-double-float
         (rewrite-as
          (complex-single-float-from-complex-double-float number)))
        (complex-long-float
         (rewrite-as
          (complex-single-float-from-complex-long-float number)))
        (t
         (rewrite-default (ntype 'complex-single-float)))))))

(declaim (inline coerce-to-complex-double-float))
(defun coerce-to-complex-double-float (number)
  (coerce number 'complex-double-float))

(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-double-float) (complex-double-float) (double-float))
(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-complex-short-float) (complex-double-float) (complex-short-float))
(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-complex-single-float) (complex-double-float) (complex-single-float))
(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-complex-long-float) (complex-double-float) (complex-long-float))

(define-rule coerce-to-complex-double-float (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (coerce-to-complex-double-float (ntype number))
      (ntype-subtypecase ntype
        (short-float
         (rewrite-as
          (complex-double-float-from-double-float
           (double-float-from-short-float number))))
        (single-float
         (rewrite-as
          (complex-double-float-from-double-float
           (double-float-from-single-float number))))
        (double-float
         (rewrite-as
          (complex-double-float-from-double-float number)))
        (long-float
         (rewrite-as
          (complex-double-float-from-double-float
           (double-float-from-long-float number))))
        (complex-double-float
         (rewrite-as number))
        (complex-short-float
         (rewrite-as
          (complex-double-float-from-complex-short-float number)))
        (complex-single-float
         (rewrite-as
          (complex-double-float-from-complex-single-float number)))
        (complex-long-float
         (rewrite-as
          (complex-double-float-from-complex-long-float number)))
        (t
         (rewrite-default (ntype 'complex-double-float)))))))

(declaim (inline coerce-to-complex-long-float))
(defun coerce-to-complex-long-float (number)
  (coerce number 'complex-long-float))

(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-long-float) (complex-long-float) (long-float))
(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-complex-short-float) (complex-long-float) (complex-short-float))
(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-complex-single-float) (complex-long-float) (complex-single-float))
(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-complex-double-float) (complex-long-float) (complex-double-float))

(define-rule coerce-to-complex-long-float (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (coerce-to-complex-long-float (ntype number))
      (ntype-subtypecase ntype
        (short-float
         (rewrite-as
          (complex-long-float-from-long-float
           (long-float-from-short-float number))))
        (single-float
         (rewrite-as
          (complex-long-float-from-long-float
           (long-float-from-single-float number))))
        (double-float
         (rewrite-as
          (complex-long-float-from-long-float
           (long-float-from-double-float number))))
        (long-float
         (rewrite-as
          (complex-long-float-from-long-float number)))
        (complex-long-float
         (rewrite-as number))
        (complex-short-float
         (rewrite-as
          (complex-long-float-from-complex-short-float number)))
        (complex-single-float
         (rewrite-as
          (complex-long-float-from-complex-single-float number)))
        (complex-double-float
         (rewrite-as
          (complex-long-float-from-complex-double-float number)))
        (t
         (rewrite-default (ntype 'complex-long-float)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FLOAT

(define-rule float (number &optional (prototype nil prototype-supplied-p))
  (if prototype-supplied-p
      (ntype-subtypecase (wrapper-ntype prototype)
        ((not float) (abort-specialization))
        (short-float (rewrite-as (coerce-to-short-float number)))
        (single-float (rewrite-as (coerce-to-single-float number)))
        (double-float (rewrite-as (coerce-to-double-float number)))
        (long-float (rewrite-as (coerce-to-long-float number)))
        (t (rewrite-default (ntype 'float))))
      (ntype-subtypecase (wrapper-ntype number)
        ((not real) (abort-specialization))
        (float (rewrite-as number))
        ((not float) (rewrite-as (coerce-to-single-float number)))
        (t (rewrite-default (ntype 'float))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COERCE

(define-rule coerce (object type-specifier)
  (let ((object-ntype (wrapper-ntype object))
        (type-specifier-ntype (wrapper-ntype type-specifier)))
    (with-constant-folding (coerce (object-ntype t)
                                   (type-specifier-ntype type-specifier))
      (if (eql-ntype-p type-specifier-ntype)
          (let ((result-ntype (ntype type-specifier-ntype)))
            (if (ntype-subtypep object-ntype result-ntype)
                (rewrite-as object)
                (ntype-subtypecase result-ntype
                  (short-float
                   (rewrite-as
                    (coerce-to-short-float object)))
                  (single-float
                   (rewrite-as
                    (coerce-to-single-float object)))
                  (double-float
                   (rewrite-as
                    (coerce-to-double-float object)))
                  (long-float
                   (rewrite-as
                    (coerce-to-long-float object)))
                  (complex-short-float
                   (rewrite-as
                    (coerce-to-complex-short-float object)))
                  (complex-single-float
                   (rewrite-as
                    (coerce-to-complex-single-float object)))
                  (complex-double-float
                   (rewrite-as
                    (coerce-to-complex-double-float object)))
                  (complex-long-float
                   (rewrite-as
                    (coerce-to-complex-long-float object)))
                  (float
                   (ntype-subtypecase object-ntype
                     ((not real) (abort-specialization))
                     (rational
                      (rewrite-as
                       (coerce-to-single-float object)))
                     (float
                      (rewrite-as object))
                     (t
                      (rewrite-default (ntype 'float)))))
                  (complex
                   (ntype-subtypecase object-ntype
                     ((not number) (abort-specialization))
                     (short-float
                      (rewrite-as
                       (coerce-to-complex-short-float object)))
                     (single-float
                      (rewrite-as
                       (coerce-to-complex-single-float object)))
                     (double-float
                      (rewrite-as
                       (coerce-to-complex-double-float object)))
                     (long-float
                      (rewrite-as
                       (coerce-to-complex-long-float object)))
                     (t (rewrite-default 'complex))))
                  (t
                   (rewrite-default result-ntype)))))
          (rewrite-default (ntype 't))))))
