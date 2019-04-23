;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(deftype function-name ()
  '(or symbol
    (cons (eql setf) (cons symbol nil))))

(define-type-inference-rule apply (function &rest args)
  (check-type-code function function)
  (if (null args)
      (type-code-from-type-specifier 'nil)
      (type-code-from-type-specifier 't)))

(define-type-inference-rule fdefinition (name)
  (check-type-code name function-name)
  (type-code-from-type-specifier 't))

(define-type-inference-rule fboundp (name)
  (check-type-code name function-name)
  (type-code-from-type-specifier 't))

(define-type-inference-rule fmakunbound (name)
  (check-type-code name function-name)
  (type-code-from-type-specifier 'function-name))

(define-type-inference-rule funcall (function &rest args)
  (check-type-code function function)
  (dolist (arg args)
    (check-type-code arg t))
  (type-code-from-type-specifier 't))

(define-type-inference-rule function-lambda-expression (function)
  (check-type-code function function)
  (type-code-from-type-specifier 't))

(define-type-inference-rule functionp (object)
  (check-type-code object t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule compiled-function-p (object)
  (check-type-code object t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule not (object)
  (check-type-code object t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule eq (x y)
  (check-type-code x t)
  (check-type-code y t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule eql (x y)
  (check-type-code x t)
  (check-type-code y t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule equal (x y)
  (check-type-code x t)
  (check-type-code y t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule equalp (x y)
  (check-type-code x t)
  (check-type-code y t)
  (type-code-from-type-specifier 't))

(define-type-inference-rule identity (object)
  (check-type-code object t)
  object)

(define-type-inference-rule complement (function)
  (check-type-code function function)
  (type-code-from-type-specifier 't))

(define-type-inference-rule constantly (value)
  (check-type-code value t)
  (type-code-from-type-specifier 'function))

(define-type-inference-rule every (predicate &rest sequences)
  (check-type-code predicate function)
  (dolist (sequence sequences)
    (check-type-code sequence sequence))
  (if (null sequences)
      (type-code-from-type-specifier 'nil)
      (type-code-from-type-specifier 'function)))

(define-type-inference-rule some (predicate &rest sequences)
  (check-type-code predicate function)
  (dolist (sequence sequences)
    (check-type-code sequence sequence))
  (if (null sequences)
      (type-code-from-type-specifier 'nil)
      (type-code-from-type-specifier 'function)))

(define-type-inference-rule notevery (predicate &rest sequences)
  (check-type-code predicate function)
  (dolist (sequence sequences)
    (check-type-code sequence sequence))
  (if (null sequences)
      (type-code-from-type-specifier 'nil)
      (type-code-from-type-specifier 'function)))

(define-type-inference-rule notany (predicate &rest sequences)
  (check-type-code predicate function)
  (dolist (sequence sequences)
    (check-type-code sequence sequence))
  (if (null sequences)
      (type-code-from-type-specifier 'nil)
      (type-code-from-type-specifier 'function)))

(define-type-inference-rule values (&rest objects)
  (dolist (object objects)
    (check-type-code object t))
  (values-list objects))

(define-type-inference-rule values-list (list)
  (check-type-code list list)
  (type-code-from-type-specifier 't))

(define-type-inference-rule get-setf-expansion (place &optional (environment +universal-type-code+))
  (check-type-code place t)
  (check-type-code environment t)
  (values
   (type-code-from-type-specifier 't)
   (type-code-from-type-specifier 't)
   (type-code-from-type-specifier 't)
   (type-code-from-type-specifier 't)
   (type-code-from-type-specifier 't)))
