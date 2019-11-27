;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defun unbound-special-function (name)
  (lambda (&rest args)
    (declare (ignore args))
    (error "The special function ~S is not bound." name)))

(defmacro define-special-function (function variable &optional (lambda-list '(&rest args)))
  (check-type variable symbol)
  (check-type function symbol)
  (let ((forward (gensym "FORWARD")))
    `(progn
       (declaim (function ,variable))
       (defvar ,variable (unbound-special-function ',function))
       (declaim (inline ,function))
       (defun ,function ,lambda-list
         (with-lambda-list-forwarding (,forward ,lambda-list)
           (,forward ,variable))))))

(define-special-function wrapper-ntype *wrapper-ntype* (wrapper))
(define-special-function wrap-constant *wrap-constant* (constant))
(define-special-function wrap-function *wrap-function* (ntypes function arguments))

(defmacro wrap (form)
  (expand-wrap form))

(defun expand-wrap (form)
  (cond ((consp form)
         `(funcall
           (specializer ',(first form))
           ,@(mapcar #'expand-wrap (rest form))))
        ((member form '(nil t))
         `(wrap-constant ,form))
        ((symbolp form)
         form)
        (t `(wrap-constant ,form))))
