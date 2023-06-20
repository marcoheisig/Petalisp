;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.codegen
  (:use
   #:common-lisp
   #:petalisp.core
   #:petalisp.ir)

  (:import-from
   #:petalisp.utilities
   #:document-variable
   #:document-function)

  (:export
   ;; blueprint.lisp
   #:kernel-blueprint
   #:iteration-space-blueprint
   #:transformation-blueprint

   ;; generic-functions.lisp
   #:compile-kernel
   #:compile-blueprint
   #:target-function
   #:source-function
   #:unpack-function
   #:store-function
   #:load-function

   ;; mixins.lisp
   #:compile-cache-mixin
   #:lisp-interpreter-mixin
   #:lisp-compiler-mixin
   #:lisp-plus-cpp-compiler-mixin
   #:cpp-compiler-mixin
   #:cuda-compiler-mixin

   ;; lisp-compiler.lisp
   #:index
   #:index+
   #:index*
   #:without-compiler-notes
   #:with-unsafe-optimization
   #:with-unsafe-optimization*
   #:with-debug-optimization
   #:unpack-array
   #:make-kernel-lambda

   ;; load-foreign-code.lisp
   #:load-foreign-code

   #:make-ir-backend))
