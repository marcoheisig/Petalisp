;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.api
  (:nicknames #:petalisp)

  (:use
   #:common-lisp
   #:petalisp.core)

  (:import-from
   #:petalisp.utilities
   #:document-compiler-macro
   #:document-function
   #:document-functions
   #:document-method-combination
   #:document-setf-expander
   #:document-structure
   #:document-type
   #:document-variable
   #:document-variables
   #:defalias)

  (:export

   ;; High-level API
   #:α
   #:a
   #:alpha
   #:α*
   #:a*
   #:alpha*
   #:β
   #:b
   #:beta
   #:β*
   #:b*
   #:beta*
   #:~
   #:~l
   #:~s
   #:~r
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
   #:array-indices
   #:shape-indices
   #:define-parallel-aliases
   #:vectorize

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
   #:range-start
   #:range-step
   #:range-last
   #:range-end
   #:range-size

   ;; Shapes
   #:shape
   #:shapep
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
   #:subdivide-arrays
   #:subdivide-shapes
   #:array-shape
   #:define-shape-syntax

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
   #:parameter
   #:optional-parameter
   #:optional-parameter-value
   #:empty-array-p
   #:element-type
   #:rank
   #:total-size
   #:input
   #:inputs
   #:value-n
   #:operator
   #:immediatep
   #:storage
   #:array-immediate
   #:range-immediate
   #:make-range-immediate
   #:lisp-datum-from-immediate

   ;; Network
   #:network
   #:network-parameters
   #:network-outputs
   #:make-network
   #:call-network
   #:differentiator

   ;; Utilities
   #:move-axis-to-front
   #:array-interior
   #:shape-interior
   #:collapse
   #:flatten
   #:slice
   #:slices
   #:stack
   #:drop-axes)

  (:shadowing-import-from :petalisp.reference-backend #:make-reference-backend)
  (:shadowing-import-from :petalisp.ir-backend #:make-ir-backend)
  (:shadowing-import-from :petalisp.native-backend #:make-native-backend))

(in-package #:petalisp.api)

(defvar *backend* (make-native-backend))
