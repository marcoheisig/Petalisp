;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
   #:compute-list-of-arrays
   #:schedule
   #:schedule-list-of-arrays
   #:wait
   #:prepare
   #:prepare-list-of-arrays
   #:array-indices
   #:shape-indices
   #:define-parallel-aliases
   #:vectorize

   ;; Backends
   #:*backend*
   #:with-backend
   #:make-reference-backend
   #:make-ir-backend
   #:make-native-backend
   #:make-multicore-backend

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
   #:shape-dimensions
   #:map-shape
   #:shape-contains
   #:shrink-shape
   #:enlarge-shape
   #:shape-union
   #:subdivide-arrays
   #:subdivide-shapes
   #:array-shape
   #:define-shape-syntax
   #:shape-table
   #:shape-table-p
   #:make-shape-table
   #:shape-table-value
   #:remove-shape-table-entry
   #:clear-shape-table

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
   #:lazy-array-input
   #:lazy-array-inputs
   #:immediatep
   #:lazy-multiple-value-ref-value-n
   #:lazy-map-operator
   #:lazy-map-number-of-values
   #:array-immediate-storage
   #:array-immediate
   #:range-immediate
   #:make-range-immediate

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

  (:shadowing-import-from :petalisp.ir #:make-ir-backend)
  (:shadowing-import-from :petalisp.native-backend #:make-native-backend)
  (:shadowing-import-from :petalisp.multicore-backend #:make-multicore-backend))

(in-package #:petalisp.api)

(defvar *backend* (make-multicore-backend))
