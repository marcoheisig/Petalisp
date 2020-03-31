;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-specializer apply (function arg &rest more-args)
  (check-ntype function function-designator)
  (let ((tail (if (null more-args)
                  arg
                  (car (last more-args)))))
    (ntype-subtypecase (wrapper-ntype tail)
      ((not list) (abort-specialization))
      (null (apply (specializer 'funcall) function arg (butlast more-args)))
      ;; We give up here, because we cannot determine the number of values
      ;; returned by APPLY.
      (t (give-up-specialization)))))

(define-specializer fdefinition (name)
  (check-ntype name function-name)
  (wrap-default (ntype 't)))

(define-specializer fboundp (name)
  (check-ntype name function-name)
  (wrap-default (ntype 't)))

(define-specializer fmakunbound (name)
  (check-ntype name function-name)
  (wrap-default (ntype 'function-name)))

(define-specializer funcall (function &rest arguments)
  (let ((function-ntype (wrapper-ntype function)))
    (if (eql-ntype-p function-ntype)
        (apply (specializer function-ntype) arguments)
        (progn
          (check-ntype function function)
          (give-up-specialization)))))

(define-specializer function-lambda-expression (function)
  (check-ntype function function)
  (wrap-default (ntype 'list)))

(define-specializer not (x)
  (let ((ntype (wrapper-ntype x)))
    (if (eql-ntype-p ntype)
        (wrap-constant (not x))
        (ntype-subtypecase ntype
          (null (wrap t))
          ((not null) (wrap nil))
          (t (wrap-default (ntype 'boolean)))))))

(define-specializer eq (a b)
  (with-constant-folding (eq ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (wrap-default
     (ntype 'generalized-boolean))))

(define-specializer eql (a b)
  (with-constant-folding (eql ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (wrap-default
     (ntype 'generalized-boolean))))

(define-specializer equal (a b)
  (with-constant-folding (equal ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (wrap-default
     (ntype 'generalized-boolean))))

(define-specializer equalp (a b)
  (with-constant-folding (equalp ((wrapper-ntype a) t) ((wrapper-ntype b) t))
    (wrap-default
     (ntype 'generalized-boolean))))

(define-specializer identity (object)
  (wrap object))

(define-specializer complement (function)
  (check-ntype function function)
  (wrap-default (ntype 'function)))

(define-specializer constantly (value)
  (wrap-default (ntype 'function)))

(define-specializer values (&rest objects)
  (wrap-function
   (mapcar #'wrapper-ntype objects)
   'values
   objects))

(define-specializer values-list (list)
  (check-ntype list list)
  (give-up-specialization))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control Flow Directives

(define-instruction (if choose) (generalized-boolean) (boolean a b)
  (ntype-subtypecase (wrapper-ntype boolean)
    (null
     (wrap b))
    ((not null)
     (wrap a))
    (t
     (wrap-default
      (ntype-union
       (wrapper-ntype a)
       (wrapper-ntype b))))))

(define-specializer if (test then &optional (else (wrap-constant nil)))
  (wrap (choose test then else)))

(define-instruction (and and-fn) (generalized-boolean) (a b)
  (ntype-subtypecase (wrapper-ntype a)
    (null
     (wrap nil))
    ((not null)
     (wrap b))
    (t
     (ntype-subtypecase (wrapper-ntype b)
       (null
        (wrap nil))
       (t
        (wrap-default
         (ntype 'generalized-boolean)))))))

(define-specializer and (&rest forms)
  (if (null forms)
      (wrap t)
      (reduce
       (lambda (a b)
         (wrap (and-fn a b)))
       forms)))

(define-instruction (or or-fn) (generalized-boolean) (a b)
  (ntype-subtypecase (wrapper-ntype a)
    ((not null)
     (wrap a))
    (null
     (wrap b))
    (t
     (wrap-default
      (ntype 'generalized-boolean)))))

(define-specializer or (&rest forms)
  (if (null forms)
      (wrap nil)
      (reduce
       (lambda (a b)
         (wrap (or-fn a b)))
       forms)))

(define-instruction (prog2 prog2-fn) (t) (a b)
  (wrap-function
   (list-ntypes (wrapper-ntype b))
   'prog2-fn
   (list a b)))
