;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching of Compiled Blueprints

(defclass compile-cache-mixin ()
  ((%compile-cache
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
  (lisp-compile-blueprint client blueprint))

(defclass lisp-plus-cpp-compiler-mixin ()
  ())

(defmethod compile-blueprint ((client lisp-plus-cpp-compiler-mixin) blueprint)
  (if (blueprint-cpp-translatable-p blueprint)
      (cpp-compile-blueprint client blueprint)
      (lisp-compile-blueprint client blueprint)))

(defclass cpp-compiler-mixin ()
  ())

(defmethod compile-blueprint ((client cpp-compiler-mixin) blueprint)
  (cpp-compile-blueprint client blueprint))

(defclass cuda-compiler-mixin ()
  ())

(defmethod compile-blueprint ((client cuda-compiler-mixin) blueprint)
  (cuda-compile-blueprint client blueprint))
