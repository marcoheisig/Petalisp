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
   #:lazy-array-indices
   #:lazy-array-interior
   #:lazy-broadcast-to
   #:lazy-broadcast-arrays
   #:lazy-broadcast-list-of-arrays
   #:lazy-collapse
   #:lazy-drop-axes
   #:lazy-flatten
   #:lazy-fuse
   #:lazy
   #:lazy-multiple-value
   #:lazy-overwrite
   #:lazy-reduce
   #:lazy-allreduce
   #:lazy-reshape
   #:lazy-shape-indices
   #:lazy-slice
   #:lazy-slices
   #:lazy-stack
   #:~
   #:~l
   #:~s
   #:~r
   #:transform
   #:to
   #:transform-sequence
   #:transform-shape
   #:transform-axis
   #:transform-lazy-array
   #:compute
   #:compute-list-of-arrays
   #:schedule
   #:schedule-list-of-arrays
   #:wait
   #:prepare
   #:prepare-list-of-arrays
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
   #:shape-interior
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
   #:empty-array-p
   #:immediatep
   #:parameter
   #:optional-parameter
   #:optional-parameter-value
   #:element-type
   #:rank
   #:total-size
   #:lazy-array-input
   #:lazy-array-inputs
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
   )

  (:shadowing-import-from :petalisp.ir #:make-ir-backend)
  (:shadowing-import-from :petalisp.native-backend #:make-native-backend)
  (:shadowing-import-from :petalisp.multicore-backend #:make-multicore-backend))

(in-package #:petalisp.api)

(defvar *backend* (make-multicore-backend))
