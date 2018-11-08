;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; We need this macro because our code generator can only handle forms
;;; that are flat and would thus destroy (SETF (AREF ...) ...) forms by
;;; lifting the AREF subform.
(defmacro store (value array row-major-index)
  `(setf (row-major-aref ,array ,row-major-index)
         ,value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler "Gensyms"

(defmacro define-compiler-gensym (name)
  (let* ((prefix '#:-symbol)
         (function-name (symbolicate name prefix)))
    `(defun ,function-name (n)
       (petalisp-memoization:with-vector-memoization (n)
         (format-symbol :petalisp-native-backend "~A-~D" ',name n)))))

(define-compiler-gensym index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with addresses and loops

(defun fixnum-+ (&rest fixnums)
  (apply #'+ fixnums))

(defun fixnum-* (&rest fixnums)
  (apply #'* fixnums))

(define-compiler-macro fixnum-+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms collect `(the fixnum ,form)))))

(define-compiler-macro fixnum-* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms collect `(the fixnum ,form)))))

(defun i+ (&rest expressions)
  (trivia:match (remove 0 expressions)
    ((list) 0)
    ((list expression) expression)
    (expressions `(fixnum-+ ,@expressions))))

(defun i* (&rest expressions)
  (trivia:match (remove 1 expressions)
    ((list) 1)
    ((list expression) expression)
    ((trivia:guard expressions (member 0 expressions)) 0)
    (expressions `(fixnum-* ,@expressions))))
