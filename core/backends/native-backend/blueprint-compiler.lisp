;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The kernel compiler

(defgeneric compile-blueprint (blueprint backend))

(defmethod compile-blueprint (blueprint (backend backend))
  (let ((lambda-expression
          (lambda-expression-from-translation-unit
           (translation-unit-from-blueprint blueprint))))
    (compile nil lambda-expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching of Compiled Kernels

(defclass compile-cache-mixin ()
  ((%compile-cache :initform (make-hash-table :test #'eq)
                   :reader compile-cache)))

(defmethod compile-blueprint (blueprint (compile-cache-mixin compile-cache-mixin))
  (petalisp-memoization:with-hash-table-memoization (blueprint)
      (compile-cache compile-cache-mixin)
    (call-next-method)))
