;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/type-inference/numbers
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/type-inference/inference))

(in-package :petalisp/core/type-inference/numbers)

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

(register-type-inference-function '+ #'standard-numeric-type-inferrer)
(register-type-inference-function '- #'standard-numeric-type-inferrer)
(register-type-inference-function '* #'standard-numeric-type-inferrer)
(register-type-inference-function '/ #'standard-numeric-type-inferrer)
