;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
   #:document-variables)

  (:export

   ;; High-level API
   #:lazy-broadcast
   #:lazy-broadcast-list-of-arrays
   #:lazy-drop-axes
   #:lazy-fuse
   #:lazy
   #:lazy-multiple-value
   #:lazy-overwrite
   #:lazy-reduce
   #:lazy-multireduce
   #:lazy-reshape
   #:lazy-index-components
   #:lazy-slice
   #:lazy-slices
   #:lazy-stack
   #:transform
   #:to
   #:transform-index
   #:transform-shape
   #:transform-axis
   #:transform-lazy-array
   #:compute
   #:compute-list-of-arrays
   #:compute-asynchronously
   #:wait
   #:completedp
   #:evaluator

   ;; Backends
   #:*backend*
   #:with-backend
   #:make-reference-backend
   #:make-ir-backend
   #:make-native-backend

   ;; Re-exports from petalisp.core

   ;; Ranges
   #:range
   #:rangep
   #:make-range
   #:empty-range
   #:size-one-range-p
   #:split-range
   #:map-range
   #:range=
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
   #:~
   #:~*
   #:make-shape
   #:shape
   #:shapep
   #:empty-shape-p
   #:shape-rank
   #:shape-range
   #:shape-ranges
   #:shape-size
   #:shape=
   #:shape<
   #:shape-difference-list
   #:shape-intersection
   #:shape-intersectionp
   #:shape-dimensions
   #:map-shape
   #:shape-contains
   #:shrink-shape
   #:split-shape
   #:enlarge-shape
   #:shape-subseq
   #:shape-prefix
   #:shape-suffix
   #:subshapep
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
   #:shape-designator-shape

   ;; Transformations
   #:transformation
   #:transformationp
   #:identity-transformation-p
   #:transformation-input-rank
   #:transformation-output-rank
   #:transformation-input-mask
   #:transformation-output-mask
   #:transformation-scalings
   #:transformation-offsets
   #:transformation-invertiblep
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation=
   #:compose-transformations
   #:collapsing-transformation
   #:enlarge-transformation
   #:map-transformation-outputs

   ;; Lazy Arrays
   #:lazy-array
   #:lazy-array-p
   #:lazy-array-shape
   #:lazy-array-element-type
   #:lazy-array-rank
   #:lazy-array-range
   #:lazy-array-ranges
   #:lazy-array-size
   #:make-unknown

   ;; Network
   #:network
   #:network-parameters
   #:network-outputs
   #:make-network
   #:call-network
   #:differentiator

   ;; Reshapers
   #:collapsing-reshaper
   #:peeling-reshaper
   #:permuting-reshaper
   #:flattening-reshaper

   ;; Harmonization
   #:harmonized-element-type
   #:lazy-harmonize
   #:lazy-harmonize-list-of-arrays
   #:lazy-fuse-and-harmonize
   #:lazy-overwrite-and-harmonize

   ;; Utilities
   #:move-axis-to-front
   #:with-lazy-arrays)

  (:shadowing-import-from :petalisp.codegen #:make-ir-backend)
  (:shadowing-import-from :petalisp.native-backend #:make-native-backend))

(defpackage #:petalisp-user
  (:use #:common-lisp #:petalisp))

(in-package #:petalisp.api)

(defvar *backend* (make-native-backend))
