;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(defmacro define-type-code-predicate (name type)
  (flet ((matching (type)
           (lambda (x) (subtypep x type))))
    (let ((matching (remove-if-not (matching type) +types+))
          (not-matching (remove-if (matching type) +types+)))
      `(progn (declaim (inline ,name))
              ,(if (< (length matching)
                      (length not-matching))
                   `(defun ,name (type-code)
                      (case type-code
                        ,@(loop for type across matching
                                collect `(,(type-code-from-type-specifier type) t))
                        (otherwise nil)))
                   `(defun ,name (type-code)
                      (case type-code
                        ,@(loop for type across not-matching
                                collect `(,(type-code-from-type-specifier type) nil))
                        (otherwise t))))))))

(define-type-code-predicate type-code-numberp number)
(define-type-code-predicate type-code-floatp float)
(define-type-code-predicate type-code-complex-float-p (complex float))
(define-type-code-predicate type-code-integerp integer)
(define-type-code-predicate type-code-characterp character)
