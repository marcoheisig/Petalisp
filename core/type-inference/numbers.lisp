;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;; This type inference function deliberately ignores most numeric types
;;; and distinguishes primarily between subtypes of float, including
;;; complex floating point numbers.  Rational numbers are slow anyways and
;;; fast integer operations would require proper bounds tracking, which is
;;; currently not a priority.
(defun numeric-supertype (argument-types)
  (let ((complex-p nil)
        (float-p nil)
        (precision 0))
    (loop for argument-type in argument-types do
      (let ((type (atomic-type argument-type)))
        (macrolet ((merge-attributes (complex-p float-p precision)
                     `(progn ,(when complex-p `(setf complex-p t))
                             ,(when float-p `(setf float-p t))
                             (setf precision (max precision ,precision)))))
          (case type
            (short-float  (merge-attributes nil t 0))
            (single-float (merge-attributes nil t 1))
            (double-float (merge-attributes nil t 2))
            (long-float   (merge-attributes nil t 3))
            (complex-short-float  (merge-attributes t t 0))
            (complex-single-float (merge-attributes t t 1))
            (complex-double-float (merge-attributes t t 2))
            (complex-long-float   (merge-attributes t t 3))))))
    (if (not float-p)
        'number
        (aref
         (if complex-p
             #(complex-short-float complex-single-float complex-double-float complex-long-float)
             #(short-float single-float double-float long-float))
         precision))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Arithmetic Functions - CLHS Figure 12-1

(define-type-inferrer + (&rest types)
  (values (list (numeric-supertype types)) nil '() '+))

(define-type-inferrer - (&rest types)
  (values (list (numeric-supertype types) nil '() '-)))

(define-type-inferrer * (&rest types)
  (values (list (numeric-supertype types) nil '() '*)))

(define-type-inferrer / (&rest types)
  (values (list (numeric-supertype types) nil '(division-by-zero) '/)))

(define-type-inferrer 1+ (type)
  (values (list type) nil '() '1+))

(define-type-inferrer 1- (type)
  (values (list type) nil '() '1-))

(define-type-inferrer floor (number &optional (divisor 'integer))
  (values (list 'integer (numeric-supertype (list number divisor))) nil '() 'floor))

(define-type-inferrer ceiling (number &optional (divisor 'integer))
  (values (list 'integer (numeric-supertype (list number divisor))) nil '() 'ceiling))

(define-type-inferrer truncate (number &optional (divisor 'integer))
  (values (list 'integer (numeric-supertype (list number divisor))) nil '() 'truncate))

(define-type-inferrer round (number &optional (divisor 'integer))
  (values (list 'integer (numeric-supertype (list number divisor))) nil '() 'round))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exponentials, Logarithms and Trigonometry  - CLHS Figure 12-2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Numeric Comparison and Predication - CLHS Figure 12-3
