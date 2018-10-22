;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler "Gensyms"

(defmacro define-compiler-gensym (name)
  (let* ((prefix '#:-variable)
         (function-name (symbolicate name prefix)))
    `(defun ,function-name (n)
       (petalisp-memoization:with-vector-memoization (n)
         (format-symbol *package* "~A-~D" ',name n)))))

(define-compiler-gensym start)
(define-compiler-gensym step)
(define-compiler-gensym end)
(define-compiler-gensym index)
(define-compiler-gensym storage)
(define-compiler-gensym range)

;; Reduction variables.
(define-compiler-gensym left)
(define-compiler-gensym right)
(define-compiler-gensym result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with addresses and loops

(defun fixnum-+ (&rest fixnums)
  (apply #'+ fixnums))

(defun fixnum-* (&rest fixnums)
  (apply #'* fixnums))

(define-compiler-macro fixnum-+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms
                   unless (eql form 0)
                     collect `(the fixnum ,form)))))

(define-compiler-macro fixnum-* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms
                   unless (eql form 1)
                     collect `(the fixnum ,form)))))
