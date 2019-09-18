;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.api
  (:nicknames #:petalisp)
  (:use
   #:common-lisp
   #:petalisp.core)
  (:export

   ;; High-level API
   #:α
   #:a
   #:alpha
   #:β
   #:b
   #:beta
   #:β*
   #:b*
   #:beta*
   #:~
   #:broadcast
   #:broadcast-arrays
   #:broadcast-list-of-arrays
   #:reshape
   #:transform
   #:τ
   #:tau
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

   ;; Re-exports from petalisp.core

   ;; Ranges
   #:range
   #:rangep
   #:make-range
   #:size-one-range-p
   #:split-range
   #:map-range
   #:range-equal
   #:range-contains
   #:range-intersection
   #:range-intersectionp
   #:range-difference-list
   #:range-start-step-end
   #:range-start
   #:range-step
   #:range-end
   #:range-size

   ;; Shapes
   #:shape
   #:shapep
   #:make-shape
   #:shape-rank
   #:shape-ranges
   #:shape-size
   #:shape-equal
   #:shape-difference-list
   #:shape-intersection
   #:shape-intersectionp
   #:map-shape
   #:shape-contains
   #:shrink-shape
   #:enlarge-shape
   #:shape-union
   #:subdivision

   ;; Transformations
   #:transformation
   #:transformationp
   #:identity-transformation-p
   #:input-rank
   #:output-rank
   #:transformation-invertiblep
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation
   #:enlarge-transformation
   #:map-transformation-outputs

   ;; Strided Arrays
   #:lazy-array
   #:lazy-array-p
   #:empty-array-p
   #:coerce-to-lazy-array
   #:element-type
   #:rank
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

  (:shadowing-import-from :petalisp.reference-backend #:make-reference-backend)
  (:shadowing-import-from :petalisp.ir-backend #:make-ir-backend)
  (:shadowing-import-from :petalisp.native-backend #:make-native-backend))
