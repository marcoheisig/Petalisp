;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros

(defmacro nest (&rest r)
  (reduce (lambda (o i) `(,@o ,i)) r :from-end t))

(defmacro bind (variables value-form &optional body)
  `(multiple-value-bind ,variables ,value-form
     (declare (ignorable ,@variables))
     ,body))

;;; We need this macro because our code generator can only handle forms
;;; that are flat and would thus destroy (SETF (AREF ...) ...) forms by
;;; lifting the AREF subform.
(defmacro store (value array row-major-index)
  `(setf (row-major-aref ,array ,row-major-index)
         ,value))

(defun stride (array axis)
  (apply #'array-row-major-index array
         (loop for i below (array-rank array)
               collect (if (= i axis) 1 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler "Gensyms"

(defmacro define-compiler-gensym (name)
  (let* ((prefix '#:-symbol)
         (function-name (symbolicate name prefix)))
    `(defun ,function-name (n)
       (petalisp-memoization:with-vector-memoization (n)
         (format-symbol *package* "~A-~D" ',name n)))))

;; Loop indices.
(define-compiler-gensym index)

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
