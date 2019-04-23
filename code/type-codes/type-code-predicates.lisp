;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(defmacro type-code-matcher (type &environment env)
  "A type code matcher is a function that takes a type code and returns
whether all objects of this type code will also satisfy TYPE."
  (flet ((matching (x) (subtypep `(and ,x (not ,type)) nil env)))
    (let ((matching (remove-if-not #'matching +types+))
          (not-matching (remove-if #'matching +types+)))
      (if (< (length matching)
             (length not-matching))
          `(lambda (type-code)
             (case type-code
               ,@(loop for type across matching
                       collect `(,(type-code-from-type-specifier type) t))
               (otherwise nil)))
          `(lambda (type-code)
             (case type-code
               ,@(loop for type across not-matching
                       collect `(,(type-code-from-type-specifier type) nil))
               (otherwise t)))))))

(defmacro define-type-code-predicate (name type)
  `(progn
     (declaim (inline ,name))
     (defun ,name (type-code)
       (funcall (type-code-matcher ,type) type-code))))

(define-type-code-predicate type-code-numberp number)
(define-type-code-predicate type-code-floatp float)
(define-type-code-predicate type-code-complex-float-p (complex float))
(define-type-code-predicate type-code-integerp integer)
(define-type-code-predicate type-code-characterp character)

