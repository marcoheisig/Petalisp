;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(cl:defpackage #:petalisp.api
  (:nicknames :petalisp)
  (:use :common-lisp :petalisp-core)
  (:export
   ;; High-level API
   #:α
   #:β
   #:a
   #:b
   #:~
   #:broadcast-arrays
   #:broadcast-shapes
   #:reshape
   #:transform
   #:τ
   #:fuse
   #:fuse*
   #:compute
   #:schedule
   #:indices
   #:define-parallel-aliases

   ;; Backends
   #:*backend*
   #:make-reference-backend
   #:make-ir-backend
   #:make-native-backend

   ;; Re-exports from petalisp-core

   ;; Ranges
   #:range
   #:rangep
   #:unary-range-p
   #:split-range
   #:make-range
   #:range-start-step-end
   #:range-start
   #:range-step
   #:range-end

   ;; Shapes
   #:shape
   #:shapep
   #:make-shape
   #:rank
   #:ranges
   #:shape-difference-list
   #:enlarge-shape

   ;; Transformations
   #:transformation
   #:transformationp
   #:identity-transformation-p
   #:input-rank
   #:output-rank
   #:invertible-transformation-p
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation
   #:enlarge-transformation
   #:map-transformation-outputs

   ;; Strided Arrays
   #:strided-array
   #:strided-array-p
   #:coerce-to-strided-array
   #:element-type
   #:total-size
   #:refcount
   #:input
   #:inputs
   #:application
   #:value-n
   #:operator
   #:reduction
   #:reduction-range
   #:fusion
   #:reference
   #:immediate
   #:immediatep
   #:storage
   #:array-immediate
   #:range-immediate
   #:make-range-immediate
   #:lisp-datum-from-immediate)

  (:shadowing-import-from :petalisp-core #:set-difference)
  (:shadowing-import-from :petalisp-reference-backend #:make-reference-backend)
  (:shadowing-import-from :petalisp-ir-backend #:make-ir-backend)
  (:shadowing-import-from :petalisp-native-backend #:make-native-backend))
