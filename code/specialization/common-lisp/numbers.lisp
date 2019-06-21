;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

(defun slow-numeric-contagion (&rest type-codes)
  (labels ((initial-state ()
             (type-code-subtypecase (pop type-codes)
               ((not number) (type-code-from-type-specifier 'nil))
               (short-float (short-float-state))
               (single-float (single-float-state))
               (double-float (double-float-state))
               (long-float (long-float-state))
               (float (float-state))
               (integer (integer-state))
               (rational (rational-state))
               (real (real-state))
               ((complex short-float) (complex-short-float-state))
               ((complex single-float) (complex-single-float-state))
               ((complex double-float) (complex-double-float-state))
               ((complex long-float) (complex-long-float-state))
               (complex (complex-state))
               (t (number-state))))
           (short-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'short-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (short-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (short-float-state))
                   (rational (short-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (single-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'single-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (single-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (single-float-state))
                   (rational (single-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-single-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (double-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'double-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (double-float-state))
                   (single-float (double-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (double-float-state))
                   (rational (double-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-double-float-state))
                   ((complex single-float) (complex-double-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (long-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'long-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (long-float-state))
                   (single-float (long-float-state))
                   (double-float (long-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (long-float-state))
                   (rational (long-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-long-float-state))
                   ((complex single-float) (complex-long-float-state))
                   ((complex double-float) (complex-long-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (float-state))
                   (single-float (float-state))
                   (double-float (float-state))
                   (long-float (float-state))
                   (float (float-state))
                   (integer (float-state))
                   (rational (float-state))
                   (real (real-state))
                   ((complex short-float) (complex-state))
                   ((complex single-float) (complex-state))
                   ((complex double-float) (complex-state))
                   ((complex long-float) (complex-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (integer-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'integer)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (short-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (integer-state))
                   (rational (rational-state))
                   (real (real-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (rational-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'rational)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (short-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (rational-state))
                   (rational (rational-state))
                   (real (real-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (real-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'real)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (real-state))
                   (single-float (real-state))
                   (double-float (real-state))
                   (long-float (real-state))
                   (float (real-state))
                   (integer (real-state))
                   (rational (real-state))
                   (real (real-state))
                   ((complex short-float) (complex-state))
                   ((complex single-float) (complex-state))
                   ((complex double-float) (complex-state))
                   ((complex long-float) (complex-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-short-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex short-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (complex-short-float-state))
                   (single-float (complex-single-float-state))
                   (double-float (complex-double-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-short-float-state))
                   (rational (complex-short-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-single-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex single-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (complex-single-float-state))
                   (single-float (complex-single-float-state))
                   (double-float (complex-double-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-single-float-state))
                   (rational (complex-single-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-single-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-double-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex double-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (complex-double-float-state))
                   (single-float (complex-double-float-state))
                   (double-float (complex-double-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-double-float-state))
                   (rational (complex-double-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-double-float-state))
                   ((complex single-float) (complex-double-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-long-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex long-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (short-float (complex-long-float-state))
                   (single-float (complex-long-float-state))
                   (double-float (complex-long-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-long-float-state))
                   (rational (complex-long-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-long-float-state))
                   ((complex single-float) (complex-long-float-state))
                   ((complex double-float) (complex-long-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'complex)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (float (complex-state))
                   (integer (complex-state))
                   (rational (complex-state))
                   (real (complex-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (number-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'number)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (type-code-from-type-specifier 'nil))
                   (t (number-state))))))
    (initial-state)))

(defun numeric-contagion (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (slow-numeric-contagion type-code-1 type-code-2)))

(defun complex-part-type-code (type-code)
  (type-code-subtypecase type-code
    ((not complex) (type-code-from-type-specifier 'nil))
    ((complex short-float) (type-code-from-type-specifier 'short-float))
    ((complex single-float) (type-code-from-type-specifier 'single-float))
    ((complex double-float) (type-code-from-type-specifier 'double-float))
    ((complex long-float) (type-code-from-type-specifier 'long-float))
    (t (type-code-from-type-specifier 'real))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addition

(defop (+ add-integers) (integer) (integer integer))
(defop (+ add-rationals) (rational) (rational rational))
(defop (+ add-short-floats) (short-float) (short-float short-float))
(defop (+ add-single-floats) (single-float) (single-float single-float))
(defop (+ add-double-floats) (double-float) (double-float double-float))
(defop (+ add-long-floats) (long-float) (long-float long-float))
(defop (+ add-complex-short-floats) (complex-short-float) (complex-short-float complex-short-float))
(defop (+ add-complex-single-floats) (complex-single-float) (complex-single-float complex-single-float))
(defop (+ add-complex-double-floats) (complex-double-float) (complex-double-float complex-double-float))
(defop (+ add-complex-long-floats) (complex-long-float) (complex-long-float complex-long-float))

(defop (+ add) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (add-integers
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (add-rationals
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (add-short-floats
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (add-single-floats
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (add-double-floats
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (add-long-floats
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (add-complex-short-floats
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (add-complex-single-floats
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (add-complex-double-floats
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (add-complex-long-floats
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))))

(define-external-rewrite-rule + (&rest numbers)
  (if (null numbers)
      (rewrite-let () 0)
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (rewrite-as (the-number number)))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (add a b)))))
        (values type-codes value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtraction

(defop (- subtract-integers) (integer) (integer integer))
(defop (- subtract-rationals) (rational) (rational rational))
(defop (- subtract-short-floats) (short-float) (short-float short-float))
(defop (- subtract-single-floats) (single-float) (single-float single-float))
(defop (- subtract-double-floats) (double-float) (double-float double-float))
(defop (- subtract-long-floats) (long-float) (long-float long-float))
(defop (- subtract-complex-short-floats) (complex-short-float) (complex-short-float complex-short-float))
(defop (- subtract-complex-single-floats) (complex-single-float) (complex-single-float complex-single-float))
(defop (- subtract-complex-double-floats) (complex-double-float) (complex-double-float complex-double-float))
(defop (- subtract-complex-long-floats) (complex-long-float) (complex-long-float complex-long-float))

(defop (- subtract) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (subtract-integers
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (subtract-rationals
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (subtract-short-floats
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (subtract-single-floats
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (subtract-double-floats
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (subtract-long-floats
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (subtract-complex-short-floats
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (subtract-complex-single-floats
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (subtract-complex-double-floats
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (subtract-complex-long-floats
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))))

(defop (- negate-integer) (integer) (integer))
(defop (- negate-rational) (rational) (rational))
(defop (- negate-short-float) (short-float) (short-float))
(defop (- negate-single-float) (single-float) (single-float))
(defop (- negate-double-float) (double-float) (double-float))
(defop (- negate-long-float) (long-float) (long-float))
(defop (- negate-complex-short-float) (complex-short-float) (complex-short-float))
(defop (- negate-complex-single-float) (complex-single-float) (complex-single-float))
(defop (- negate-complex-double-float) (complex-double-float) (complex-double-float))
(defop (- negate-complex-long-float) (complex-long-float) (complex-long-float))
(defop (- negate) (number) (number) (x)
  (type-code-subtypecase x
    ((not number) (abort-specialization))
    (integer (rewrite-as (negate-integer x)))
    (rational (rewrite-as (negate-rational x)))
    (short-float (rewrite-as (negate-short-float x)))
    (single-float (rewrite-as (negate-single-float x)))
    (double-float (rewrite-as (negate-double-float x)))
    (long-float (rewrite-as (negate-long-float x)))
    (complex-short-float (rewrite-as (negate-complex-short-float x)))
    (complex-single-float (rewrite-as (negate-complex-single-float x)))
    (complex-double-float (rewrite-as (negate-complex-double-float x)))
    (complex-long-float (rewrite-as (negate-complex-long-float x)))))

(define-external-rewrite-rule - (&rest numbers)
  (if (null numbers)
      (abort-specialization)
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (if (null numbers)
                (rewrite-as (negate number))
                (rewrite-as (the-number number))))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (subtract a b)))))
        (values type-codes value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiplication

(defop (* multiply-integers) (integer) (integer integer))
(defop (* multiply-rationals) (rational) (rational rational))
(defop (* multiply-short-floats) (short-float) (short-float short-float))
(defop (* multiply-single-floats) (single-float) (single-float single-float))
(defop (* multiply-double-floats) (double-float) (double-float double-float))
(defop (* multiply-long-floats) (long-float) (long-float long-float))
(defop (* multiply-complex-short-floats) (complex-short-float) (complex-short-float complex-short-float))
(defop (* multiply-complex-single-floats) (complex-single-float) (complex-single-float complex-single-float))
(defop (* multiply-complex-double-floats) (complex-double-float) (complex-double-float complex-double-float))
(defop (* multiply-complex-long-floats) (complex-long-float) (complex-long-float complex-long-float))

(defop (* multiply) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (multiply-integers
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (multiply-rationals
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (multiply-short-floats
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (multiply-single-floats
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (multiply-double-floats
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (multiply-long-floats
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (multiply-complex-short-floats
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (multiply-complex-single-floats
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (multiply-complex-double-floats
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (multiply-complex-long-floats
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))))

(define-external-rewrite-rule * (&rest numbers)
  (if (null numbers)
      (rewrite-let () 1)
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (rewrite-as (the-number number)))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (multiply a b)))))
        (values type-codes value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Division

(defop (/ divide-integers) (integer) (integer integer))
(defop (/ divide-rationals) (rational) (rational rational))
(defop (/ divide-short-floats) (short-float) (short-float short-float))
(defop (/ divide-single-floats) (single-float) (single-float single-float))
(defop (/ divide-double-floats) (double-float) (double-float double-float))
(defop (/ divide-long-floats) (long-float) (long-float long-float))
(defop (/ divide-complex-short-floats) (complex-short-float) (complex-short-float complex-short-float))
(defop (/ divide-complex-single-floats) (complex-single-float) (complex-single-float complex-single-float))
(defop (/ divide-complex-double-floats) (complex-double-float) (complex-double-float complex-double-float))
(defop (/ divide-complex-long-floats) (complex-long-float) (complex-long-float complex-long-float))

(defop (/ divide) (number) (number number) (a b)
  (type-code-subtypecase (numeric-contagion a b)
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (divide-integers
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (divide-rationals
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (divide-short-floats
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (divide-single-floats
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (divide-double-floats
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (divide-long-floats
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (divide-complex-short-floats
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (divide-complex-single-floats
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (divide-complex-double-floats
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (divide-complex-long-floats
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))))

(define-external-rewrite-rule / (&rest numbers)
  (if (null numbers)
      (rewrite-let () 1)
      (multiple-value-bind (type-codes value)
          (rewrite-let ((number (process-argument (pop numbers))))
            (rewrite-as (the-number number)))
        (loop while numbers do
          (multiple-value-setq (type-codes value)
            (rewrite-let ((a (values type-codes value))
                          (b (process-argument (pop numbers))))
              (rewrite-as (divide a b)))))
        (values type-codes value))))
