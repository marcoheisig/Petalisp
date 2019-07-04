;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(define-external-rewrite-rule apply (function arg &rest more-args)
  (check-argument function function)
  (check-argument (car (last (cons arg more-args))) list)
  (type-codes))

(define-rewrite-rules fdefinition (t) (name)
  (check-type-code name function-name))

(define-rewrite-rules fboundp (t) (name)
  (check-type-code name function-name))

(define-rewrite-rules fmakunbound (function-name) (name)
  (check-type-code name function-name))

(define-external-rewrite-rule funcall (function &rest args)
  (check-argument function function)
  (dolist (arg args)
    (check-argument arg t))
  (type-codes))

(define-rewrite-rules function-lambda-expression (list) (function)
  (check-type-code function function))

(define-rewrite-rules functionp (generalized-boolean) (object)
  (type-code-subtypecase object
    ((not function) (rewrite-as nil))
    (function (rewrite-as t))))

(define-rewrite-rules compiled-function-p (generalized-boolean) (object)
  (type-code-subtypecase object
    ((not compiled-function) (rewrite-as nil))
    (compiled-function (rewrite-as t))))

(define-rewrite-rules not (boolean) (x)
  (type-code-subtypecase x
    (null (rewrite-as t))
    ((not null) (rewrite-as nil))))

(define-rewrite-rules eq (generalized-boolean) (a b))

(define-rewrite-rules eql (generalized-boolean) (a b))

(define-rewrite-rules equal (generalized-boolean) (a b))

(define-rewrite-rules equalp (generalized-boolean) (a b))

(define-rewrite-rules identity (t) (object)
  (rewrite-as object))

(define-rewrite-rules complement (function) (function)
  (check-type-code function function))

(define-rewrite-rules constantly (function) (value))

(define-external-rewrite-rule values (&rest objects)
  (let ((type-codes '())
        (values '()))
    (loop for object in objects do
      (multiple-value-bind (type-code value)
          (%process-argument object)
        (push type-code type-codes)
        (push value values)))
    (apply
     #'process-call
     (nreverse type-codes)
     'values
     (nreverse values))))

(define-rewrite-rules values-list () (list)
  (check-type-code list list))
