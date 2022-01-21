;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.type-inference
  (:use #:common-lisp)
  (:export
   ;; Utilities.
   #:function-arity

   ;; Auxiliary types.
   #:function-name
   #:function-designator
   #:character-designator
   #:string-designator
   #:package-designator
   #:radix
   #:character-code
   #:arity
   #:argument-index
   #:byte-specifier
   #:complex-short-float
   #:complex-single-float
   #:complex-double-float
   #:complex-long-float
   #:generalized-boolean
   #:multiple-value-count
   #:type-specifier

   ;; Ntype manipulation.
   #:*ntypes*
   #:ntype
   #:ntype-bits
   #:ntype-id
   #:ntype=
   #:ntype<
   #:type-specifier
   #:ntype-of
   #:generalize-ntype
   #:with-ntype-caching
   #:ntype-subtypecase
   #:array-element-ntype
   #:make-rank-zero-array
   #:empty-ntype-p
   #:universal-ntype-p
   #:eql-ntype-p
   #:ntype-union
   #:ntype-subtypep
   #:ntype-subtypepc2
   #:list-ntypes

   ;; Type inference.
   #:abort-specialization
   #:give-up-specialization
   #:check-ntype
   #:define-specializer
   #:define-differentiator
   #:wrap
   #:wrap-default
   #:wrapper-ntype
   #:wrap-constant
   #:wrap-function
   #:define-instruction
   #:define-simple-instruction
   #:specialize
   #:differentiate
   #:infer-ntypes

   ;; Type checks.

   #:the-number
   #:the-real
   #:the-rational
   #:the-integer
   #:the-float
   #:the-short-float
   #:the-single-float
   #:the-double-float
   #:the-long-float
   #:the-complex
   #:the-complex-short-float
   #:the-complex-single-float
   #:the-complex-double-float
   #:the-complex-long-float
   #:the-function
   #:the-character
   #:the-symbol

   ;; Specialized functions.

   ;; abs.lisp
   #:short-float-abs
   #:single-float-abs
   #:double-float-abs
   #:long-float-abs
   #:complex-short-float-abs
   #:complex-single-float-abs
   #:complex-double-float-abs
   #:complex-long-float-abs
   ;; add.lisp
   #:short-float+
   #:single-float+
   #:double-float+
   #:long-float+
   #:complex-short-float+
   #:complex-single-float+
   #:complex-double-float+
   #:complex-long-float+
   ;; auxiliary.lisp
   #:and-fn
   #:or-fn
   #:prog2-fn
   ;; casts.lisp
   #:short-float-from-single-float
   #:short-float-from-double-float
   #:short-float-from-long-float
   #:single-float-from-short-float
   #:single-float-from-double-float
   #:single-float-from-long-float
   #:double-float-from-short-float
   #:double-float-from-single-float
   #:double-float-from-long-float
   #:long-float-from-short-float
   #:long-float-from-single-float
   #:long-float-from-double-float
   #:complex-short-float-from-short-float
   #:complex-short-float-from-complex-single-float
   #:complex-short-float-from-complex-double-float
   #:complex-short-float-from-complex-long-float
   #:complex-single-float-from-single-float
   #:complex-single-float-from-complex-short-float
   #:complex-single-float-from-complex-double-float
   #:complex-single-float-from-complex-long-float
   #:complex-double-float-from-double-float
   #:complex-double-float-from-complex-short-float
   #:complex-double-float-from-complex-single-float
   #:complex-double-float-from-complex-long-float
   #:complex-long-float-from-long-float
   #:complex-long-float-from-complex-short-float
   #:complex-long-float-from-complex-single-float
   #:complex-long-float-from-complex-double-float
   ;; cmpeq.lisp
   #:short-float=
   #:single-float=
   #:double-float=
   #:long-float=
   #:complex-short-float=
   #:complex-single-float=
   #:complex-double-float=
   #:complex-long-float=
   ;; cmpneq.lisp
   #:short-float/=
   #:single-float/=
   #:double-float/=
   #:long-float/=
   #:complex-short-float/=
   #:complex-single-float/=
   #:complex-double-float/=
   #:complex-long-float/=
   ;; cmpx.lisp
   #:short-float<
   #:single-float<
   #:double-float<
   #:long-float<
   #:short-float>
   #:single-float>
   #:double-float>
   #:long-float>
   #:short-float<=
   #:single-float<=
   #:double-float<=
   #:long-float<=
   #:short-float>=
   #:single-float>=
   #:double-float>=
   #:long-float>=
   ;; complex.lisp
   #:short-float-complex
   #:single-float-complex
   #:double-float-complex
   #:long-float-complex
   #:short-float-cis
   #:single-float-cis
   #:double-float-cis
   #:long-float-cis
   #:complex-short-float-realpart
   #:complex-single-float-realpart
   #:complex-double-float-realpart
   #:complex-long-float-realpart
   #:complex-short-float-imagpart
   #:complex-single-float-imagpart
   #:complex-double-float-imagpart
   #:complex-long-float-imagpart
   ;; cos.lisp
   #:short-float-cos
   #:single-float-cos
   #:double-float-cos
   #:long-float-cos
   #:complex-short-float-cos
   #:complex-single-float-cos
   #:complex-double-float-cos
   #:complex-long-float-cos
   ;; div.lisp
   #:short-float/
   #:single-float/
   #:double-float/
   #:long-float/
   #:complex-short-float/
   #:complex-single-float/
   #:complex-double-float/
   #:complex-long-float/
   ;; max.lisp
   #:short-float-max
   #:single-float-max
   #:double-float-max
   #:long-float-max
   ;; min.lisp
   #:short-float-min
   #:single-float-min
   #:double-float-min
   #:long-float-min
   ;; mul.lisp
   #:short-float*
   #:single-float*
   #:double-float*
   #:long-float*
   #:complex-short-float*
   #:complex-single-float*
   #:complex-double-float*
   #:complex-long-float*
   ;; sin.lisp
   #:short-float-sin
   #:single-float-sin
   #:double-float-sin
   #:long-float-sin
   #:complex-short-float-sin
   #:complex-single-float-sin
   #:complex-double-float-sin
   #:complex-long-float-sin
   ;; sub.lisp
   #:short-float-
   #:single-float-
   #:double-float-
   #:long-float-
   #:complex-short-float-
   #:complex-single-float-
   #:complex-double-float-
   #:complex-long-float-
   #:short-float-unary-
   #:single-float-unary-
   #:double-float-unary-
   #:long-float-unary-
   #:complex-short-float-unary-
   #:complex-single-float-unary-
   #:complex-double-float-unary-
   #:complex-long-float-unary-
   ;; tan.lisp
   #:short-float-tan
   #:single-float-tan
   #:double-float-tan
   #:long-float-tan
   #:complex-short-float-tan
   #:complex-single-float-tan
   #:complex-double-float-tan
   #:complex-long-float-tan))
