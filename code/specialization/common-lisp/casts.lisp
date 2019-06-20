;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-Point Casts

(defun coerce-to-short-float (number)
  (coerce number 'short-float))

(defop (coerce-to-short-float short-float-from-single-float) (short-float) (single-float))
(defop (coerce-to-short-float short-float-from-double-float) (short-float) (double-float))
(defop (coerce-to-short-float short-float-from-long-float) (short-float) (long-float))

(define-rewrite-rules coerce-to-short-float (short-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (short-float (rewrite-as real))
    (single-float (rewrite-as (short-float-from-single-float real)))
    (double-float (rewrite-as (short-float-from-double-float real)))
    (long-float (rewrite-as (short-float-from-long-float real)))))

(defun coerce-to-single-float (number)
  (coerce number 'single-float))

(defop (coerce-to-single-float single-float-from-short-float) (single-float) (short-float))
(defop (coerce-to-single-float single-float-from-double-float) (single-float) (double-float))
(defop (coerce-to-single-float single-float-from-long-float) (single-float) (long-float))

(define-rewrite-rules coerce-to-single-float (single-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (single-float (rewrite-as real))
    (short-float (rewrite-as (single-float-from-short-float real)))
    (double-float (rewrite-as (single-float-from-double-float real)))
    (long-float (rewrite-as (single-float-from-long-float real)))))

(defun coerce-to-double-float (number)
  (coerce number 'double-float))

(defop (coerce-to-double-float double-float-from-short-float) (double-float) (short-float))
(defop (coerce-to-double-float double-float-from-single-float) (double-float) (single-float))
(defop (coerce-to-double-float double-float-from-long-float) (double-float) (long-float))

(define-rewrite-rules coerce-to-double-float (double-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (double-float (rewrite-as real))
    (short-float (rewrite-as (double-float-from-short-float real)))
    (single-float (rewrite-as (double-float-from-single-float real)))
    (long-float (rewrite-as (double-float-from-long-float real)))))

(defun coerce-to-long-float (number)
  (coerce number 'long-float))

(defop (coerce-to-long-float long-float-from-short-float) (long-float) (short-float))
(defop (coerce-to-long-float long-float-from-single-float) (long-float) (single-float))
(defop (coerce-to-long-float long-float-from-double-float) (long-float) (double-float))

(define-rewrite-rules coerce-to-long-float (long-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (long-float (rewrite-as real))
    (short-float (rewrite-as (long-float-from-short-float real)))
    (single-float (rewrite-as (long-float-from-single-float real)))
    (double-float (rewrite-as (long-float-from-double-float real)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Complex Floating-Point Casts

(defun coerce-to-complex-short-float (number)
  (coerce number 'complex-short-float))

(defop (coerce-to-complex-short-float complex-short-float-from-complex-single-float) (complex-short-float) (complex-single-float))
(defop (coerce-to-complex-short-float complex-short-float-from-complex-double-float) (complex-short-float) (complex-double-float))
(defop (coerce-to-complex-short-float complex-short-float-from-complex-long-float) (complex-short-float) (complex-long-float))

(define-rewrite-rules coerce-to-complex-short-float (complex-short-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (complex-short-float (rewrite-as real))
    (complex-single-float (rewrite-as (complex-short-float-from-complex-single-float real)))
    (complex-double-float (rewrite-as (complex-short-float-from-complex-double-float real)))
    (complex-long-float (rewrite-as (complex-short-float-from-complex-long-float real)))))

(defun coerce-to-complex-single-float (number)
  (coerce number 'complex-single-float))

(defop (coerce-to-complex-single-float complex-single-float-from-complex-short-float) (complex-single-float) (complex-short-float))
(defop (coerce-to-complex-single-float complex-single-float-from-complex-double-float) (complex-single-float) (complex-double-float))
(defop (coerce-to-complex-single-float complex-single-float-from-complex-long-float) (complex-single-float) (complex-long-float))

(define-rewrite-rules coerce-to-complex-single-float (complex-single-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (complex-single-float (rewrite-as real))
    (complex-short-float (rewrite-as (complex-single-float-from-complex-short-float real)))
    (complex-double-float (rewrite-as (complex-single-float-from-complex-double-float real)))
    (complex-long-float (rewrite-as (complex-single-float-from-complex-long-float real)))))

(defun coerce-to-complex-double-float (number)
  (coerce number 'complex-double-float))

(defop (coerce-to-complex-double-float complex-double-float-from-complex-short-float) (complex-double-float) (complex-short-float))
(defop (coerce-to-complex-double-float complex-double-float-from-complex-single-float) (complex-double-float) (complex-single-float))
(defop (coerce-to-complex-double-float complex-double-float-from-complex-long-float) (complex-double-float) (complex-long-float))

(define-rewrite-rules coerce-to-complex-double-float (complex-double-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (complex-double-float (rewrite-as real))
    (complex-short-float (rewrite-as (complex-double-float-from-complex-short-float real)))
    (complex-single-float (rewrite-as (complex-double-float-from-complex-single-float real)))
    (complex-long-float (rewrite-as (complex-double-float-from-complex-long-float real)))))

(defun coerce-to-complex-long-float (number)
  (coerce number 'complex-long-float))

(defop (coerce-to-complex-long-float complex-long-float-from-complex-short-float) (complex-long-float) (complex-short-float))
(defop (coerce-to-complex-long-float complex-long-float-from-complex-single-float) (complex-long-float) (complex-single-float))
(defop (coerce-to-complex-long-float complex-long-float-from-complex-double-float) (complex-long-float) (complex-double-float))

(define-rewrite-rules coerce-to-complex-long-float (complex-long-float) (real)
  (type-code-subtypecase real
    ((not real) (abort-specialization))
    (complex-long-float (rewrite-as real))
    (complex-short-float (rewrite-as (complex-long-float-from-complex-short-float real)))
    (complex-single-float (rewrite-as (complex-long-float-from-complex-single-float real)))
    (complex-double-float (rewrite-as (complex-long-float-from-complex-double-float real)))))
