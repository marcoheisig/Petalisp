;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;; The purpose of this file is to turn function calls with known argument
;;; type codes into a specialized compound form, if possible.  A
;;; specialized compound form is an s-expression whose first element is the
;;; name of a specialized function, and whose remaining elements are either
;;; non-negative integers - denoting a reference to the nth argument of the
;;; original function - or specialized compound forms.
;;;
;;; The non-negative integers at the leaves are essentially de Bruijn
;;; indices for the arguments of the function denoted by the outermost
;;; specialized compound form.
;;;
;;; Examples:
;;;
;;;  (defvar f32 (type-code-from-type-specifier 'single-float))
;;;  (defvar u16 (type-code-from-type-specifier '(unsigned-byte 16)))
;;;
;;;  (specialize #'+ (list f32 f32) (list f32))
;;;   => (f32.+ 0 1)
;;;
;;;  (specialize #'+ (list f32 f32 u16) (list f32))
;;;   => (f32.+ 0 (f32.+ 1 (f32-from-u16 2)))

(defvar *strength-reduction-functions* (make-hash-table :test #'eq))

(defun register-strength-reduction-function (fn strength-reduction-fn)
  (setf (gethash fn *strength-reduction-functions*)
        (lambda (&rest type-codes)
          (apply inference-fn type-codes))))

(defmacro define-strength-reduction-rule (name lambda-list &body body)
  "Define a strength reduction rule for the function with the supplied
NAME.  The supplied LAMBDA-LIST must accept as many arguments as the
corresponding function, while consisting only of mandatory, optional and
rest arguments.

During strength reduction, each statement in BODY will be evaluated in an
environment where each lambda list argument is bound to a type code.  It
should either return a specializer compound form, or abort the strength
reduction by calling GIVE-UP-STRENGTH-REDUCTION."
  (dolist (keyword (intersection lambda-list lambda-list-keywords))
    (unless (member keyword '(&optional &rest))
      (error "Invalid strength reduction rule keyword: ~S" keyword)))
  `(register-strength-reduction-function
    (function ,name)
    (lambda ,lambda-list ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strength Reduction

#+(or)
(defvar *simplifiction-functions* '())

#+(or)
(defmacro define-simplifier (name lambda-list)
  `())

#+(or)
(define-simplifier + (inputs)
  (trivia:ematch inputs
    ((list)
     (load-time-value (coerce-to-lazy-array 0)))
    ((list number)
     (convert-to 'number number))
    ((list* a more-numbers)
     (let* ((b (simplify #'+ more-numbers))
            (type-code (petalisp.type-codes:values-type-codes #'+ a b)))
       (make-instance 'application
         :operator operator
         :value-n 0
         :inputs (list (convert-to type-code a)
                       (convert-to type-code b))
         :shape shape
         :type-code type-code)))))

(defun specialize (function input-type-codes output-type-codes)
  function)
