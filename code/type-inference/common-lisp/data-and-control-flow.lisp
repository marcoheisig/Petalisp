;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-rule apply (function arg &rest more-args)
  (check-ntype function function-designator)
  (let ((tail (if (null more-args)
                  arg
                  (car (last more-args)))))
    (ntype-subtypecase (wrapper-ntype tail)
      ((not list) (abort-specialization))
      (null (apply (find-rule 'funcall) function arg (butlast more-args)))
      ;; We give up here, because we cannot determine the number of values
      ;; returned by APPLY.
      (t (give-up-specialization)))))

(define-rule fdefinition (name)
  (check-ntype name function-name)
  (rewrite-default (ntype 't)))

(define-rule fboundp (name)
  (check-ntype name function-name)
  (rewrite-default (ntype 't)))

(define-rule fmakunbound (name)
  (check-ntype name function-name)
  (rewrite-default (ntype 'function-name)))

(define-rule funcall (function &rest arguments)
  (let ((function-ntype (wrapper-ntype function)))
    (if (eql-ntype-p function-ntype)
        (apply (find-rule function-ntype) arguments)
        (progn
          (check-ntype function function)
          (give-up-specialization)))))

(define-rule function-lambda-expression (function)
  (check-ntype function function)
  (rewrite-default (ntype 'list)))

(define-rule not (x)
  (let ((ntype (wrapper-ntype x)))
    (if (eql-ntype-p ntype)
        (wrap-constant (not x))
        (ntype-subtypecase ntype
          (null (rewrite-as t))
          ((not null) (rewrite-as nil))
          (t (rewrite-default (ntype 'boolean)))))))

(define-rule eq (a b)
  (with-constant-folding (eq ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (rewrite-default
     (ntype 'generalized-boolean))))

(define-rule eql (a b)
  (with-constant-folding (eql ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (rewrite-default
     (ntype 'generalized-boolean))))

(define-rule equal (a b)
  (with-constant-folding (equal ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (rewrite-default
     (ntype 'generalized-boolean))))

(define-rule equalp (a b)
  (with-constant-folding (equalp ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (rewrite-default
     (ntype 'generalized-boolean))))

(define-rule identity (object)
  (rewrite-as object))

(define-rule complement (function)
  (check-ntype function function)
  (rewrite-default (ntype 'function)))

(define-rule constantly (value)
  (rewrite-default (ntype 'function)))

(define-rule values (&rest objects)
  (wrap-function
   (mapcar #'wrapper-ntype objects)
   'values
   objects))

(define-rule values-list (list)
  (check-ntype list list)
  (give-up-specialization))
