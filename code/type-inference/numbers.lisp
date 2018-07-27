;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; This type inference function deliberately ignores most numeric types
;;; and distinguishes primarily between subtypes of float, including
;;; complex floating point numbers. Rational numbers are slow anyways and
;;; fast integer operations would require proper bounds tracking, which is
;;; currently not a priority.
(defun standard-numeric-type-inferrer (argument-types)
  (let ((complex nil)
        (float nil)
        (double-precision nil))
    (declare (type (member t nil)))
    (loop for argument-type in argument-types do
      (let ((type (atomic-type argument-type)))
        (case type
          (single-float
           (setf float t))
          (double-float
           (setf float t)
           (setf double-precision t))
          (complex-single-float
           (setf float t)
           (setf complex t))
          (complex-double-float
           (setf float t)
           (setf complex t)
           (setf double-precision t)))))
    (if (not float)
        't
        (if complex
            (if double-precision
                'complex-double-float
                'complex-single-float)
            (if double-precision
                'double-float
                'single-float)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Arithmetic Functions - CLHS Figure 12-1

(register-type-inferrer '+ #'standard-numeric-type-inferrer)
(register-type-inferrer '- #'standard-numeric-type-inferrer)
(register-type-inferrer '* #'standard-numeric-type-inferrer)
(register-type-inferrer '/ #'standard-numeric-type-inferrer)
(register-type-inferrer '1+ #'standard-numeric-type-inferrer)
(register-type-inferrer '1- #'standard-numeric-type-inferrer)
(register-type-inferrer 'conjugate #'standard-numeric-type-inferrer)
(register-type-inferrer 'gcd (constantly 'integer))
(register-type-inferrer 'lcm (constantly 'integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exponentials, Logarithms and Trigonometry  - CLHS Figure 12-2

(register-type-inferrer 'abs #'standard-numeric-type-inferrer)
;;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Numeric Comparison and Predication - CLHS Figure 12-3

(register-type-inferrer '< (constantly 'boolean))
(register-type-inferrer '> (constantly 'boolean))
(register-type-inferrer '<= (constantly 'boolean))
(register-type-inferrer '>= (constantly 'boolean))
(register-type-inferrer '/= (constantly 'boolean))
(register-type-inferrer '= (constantly 'boolean))
(register-type-inferrer 'evenp (constantly 'boolean))
(register-type-inferrer 'max (constantly 'boolean))
(register-type-inferrer 'min (constantly 'boolean))
(register-type-inferrer 'minusp (constantly 'boolean))
(register-type-inferrer 'plusp (constantly 'boolean))
(register-type-inferrer 'zerop (constantly 'boolean))
(register-type-inferrer 'oddp (constantly 'boolean))
(register-type-inferrer 'evenp (constantly 'boolean))
