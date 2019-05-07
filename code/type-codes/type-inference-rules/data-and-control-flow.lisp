;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(deftype function-name ()
  '(or symbol
    (cons (eql setf) (cons symbol nil))))

(deftype function-designator ()
  '(or symbol function))

(define-type-inference-rule apply (function &rest args)
  (check-type-code function function-designator)
  (if (null args)
      (abort-type-inference)
      (give-up-type-inference)))

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
  (check-type-code function function-designator)
  (dolist (arg args)
    (check-type-code arg t))
  (give-up-type-inference))

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
  object)

(define-type-inference-rule complement (function)
  (check-type-code function function)
  (type-code-from-type-specifier 'function))

(define-type-inference-rule constantly (value)
  (check-type-code value t)
  (type-code-from-type-specifier 'function))

(define-type-inference-rule every (predicate &rest sequences)
  (check-type-code predicate function-designator)
  (if (null sequences)
      (abort-type-inference)
      (give-up-type-inference)))

(define-type-inference-rule some (predicate &rest sequences)
  (check-type-code predicate function-designator)
  (if (null sequences)
      (abort-type-inference)
      (give-up-type-inference)))

(define-type-inference-rule notevery (predicate &rest sequences)
  (check-type-code predicate function-designator)
  (if (null sequences)
      (abort-type-inference)
      (give-up-type-inference)))

(define-type-inference-rule notany (predicate &rest sequences)
  (check-type-code predicate function-designator)
  (if (null sequences)
      (abort-type-inference)
      (give-up-type-inference)))

(define-type-inference-rule values (&rest objects)
  (values-list objects))

(define-type-inference-rule values-list (list)
  (type-code-subtypecase list
    ((not list) (abort-type-inference))
    (t (give-up-type-inference))))
