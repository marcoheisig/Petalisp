;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching of Compiled Blueprints

(defclass compile-cache-mixin ()
  ((%compile-cache :reader compile-cache
                   :initform (make-hash-table :test #'eq))))

(defmethod compile-blueprint :around
    (blueprint (backend compile-cache-mixin))
  (petalisp-memoization:with-hash-table-memoization (blueprint)
      (compile-cache backend)
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler "Gensyms"

(defun symbol-with-indices (name &rest indices)
  (format-symbol *package* "~A~{-~D~}" name indices))

(defun base-index-symbol (depth reference-id)
  (symbol-with-indices "BASE-INDEX" depth reference-id))

(defun index-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "INDEX" n)))

(defun storage-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "ARRAY" n)))

(defun bound-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "BOUND" n)))

(defun accumulator-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbol-with-indices "ACC" n)))

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
