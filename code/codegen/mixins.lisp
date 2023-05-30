;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching of Compiled Blueprints

(defclass compile-cache-mixin ()
  ((compile-cache
    :initform (make-compile-cache)
    :type compile-cache
    :reader compile-cache)))

(defmethod compile-blueprint :around
    ((client compile-cache-mixin)
     blueprint)
  (alexandria:ensure-gethash
   blueprint
   (compile-cache-table (compile-cache client))
   (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation

(defclass lisp-interpreter-mixin ()
  ())

(defmethod compile-blueprint ((client lisp-interpreter-mixin) blueprint)
  #'interpret-kernel)

(defclass lisp-compiler-mixin ()
  ())

(defmethod compile-blueprint ((client lisp-compiler-mixin) blueprint)
  (compile nil (translate-blueprint client blueprint)))

(defclass lisp-or-cpp-compiler-mixin ()
  ())

(defclass cpp-compiler-mixin ()
  ())

(defclass cuda-compiler-mixin ()
  ())
