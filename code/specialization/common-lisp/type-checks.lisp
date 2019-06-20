;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defmacro define-type-check (type)
  (check-type type symbol)
  (let ((name (intern (format nil "~@:(the-~A~)" type))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (object)
         (check-type object ,type)
         object)
       (define-rewrite-rules ,name (,type) (object)
         (type-code-subtypecase object
           ((not ,type) (abort-specialization))
           (,type (rewrite-as object)))))))

(define-type-check number)
(define-type-check real)
(define-type-check rational)
(define-type-check ratio)
(define-type-check integer)
(define-type-check float)
(define-type-check short-float)
(define-type-check single-float)
(define-type-check double-float)
(define-type-check long-float)
(define-type-check complex-short-float)
(define-type-check complex-single-float)
(define-type-check complex-double-float)
(define-type-check complex-long-float)
(define-type-check function)
(define-type-check character)

