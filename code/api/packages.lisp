;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.api

  (:use #:common-lisp #:petalisp)

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
   #:broadcast
   #:lazy-fuse
   #:lazy
   #:lazy-multiple-value
   #:lazy-overwrite
   #:lazy-reduce
   #:lazy-reshape
   #:lazy-index-components
   #:lazy-rearrange
   #:lazy-slice
   #:lazy-slices
   #:lazy-sort
   #:lazy-stack
   #:transform
   #:to
   #:transform-index
   #:transform-shape
   #:transform-axis
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
   #:range-emptyp
   #:empty-range
   #:range-with-size-one-p
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
   #:shape-emptyp
   #:shape-rank
   #:shape-range
   #:shape-ranges
   #:shape-size
   #:shape=
   #:shape<
   #:shape-difference-list
   #:shape-intersection
   #:shape-intersectionp
   #:shape-dimension
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
   #:fuse-shapes
   #:subdivide-arrays
   #:subdivide-shapes
   #:array-shape
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
   #:transformation-identityp
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation=
   #:compose-transformations
   #:deflating-transformation
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
   #:lazy-array-dimension
   #:lazy-array-dimensions
   #:make-unknown

   ;; Automatic Differentiation
   #:differentiator

   ;; Reshapers
   #:deflater
   #:peeler
   #:slicer

   ;; Harmonization
   #:harmonized-element-type
   #:harmonize
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
