;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp-1.0

  (:nicknames #:petalisp)

  (:use #:common-lisp)

  ;; Ranges
  (:export
   #:range
   #:rangep
   #:range-emptyp
   #:range-with-size-one-p
   #:range-size
   #:range-start
   #:range-step
   #:range-last
   #:range-end
   #:split-range
   #:map-range
   #:range=
   #:range-contains
   #:subrangep
   #:range-intersection
   #:range-intersectionp
   #:range-difference-list)

  ;; Shapes
  (:export
   #:~
   #:~*
   #:make-shape
   #:shape-designator-shape
   #:shape
   #:shapep
   #:shape-emptyp
   #:shape-with-size-one-p
   #:shape-size
   #:shape-rank
   #:shape-range
   #:shape-ranges
   #:shape-dimension
   #:shape-dimensions
   #:shape=
   #:shape<
   #:shape-intersection
   #:shape-intersectionp
   #:shape-difference-list
   #:subdivide-shapes
   #:fuse-shapes
   #:map-shape
   #:split-shape
   #:shape-contains
   #:subshapep)

  ;; Transformations
  (:export
   #:transformation
   #:transformationp
   #:transform
   #:to
   #:make-transformation
   #:transformation-input-rank
   #:transformation-output-rank
   #:transformation-input-mask
   #:transformation-output-mask
   #:transformation-scalings
   #:transformation-offsets
   #:transformation-identityp
   #:transformation-invertiblep
   #:transformation=
   #:compose-transformations
   #:invert-transformation
   #:transform-index
   #:transform-shape)

  ;; Lazy Arrays
  (:export
   #:lazy-array
   #:lazy-array-p
   #:lazy-array-shape
   #:lazy-array-element-type
   #:lazy-array-rank
   #:lazy-array-size
   #:lazy-array-range
   #:lazy-array-ranges
   #:lazy-array-dimension
   #:lazy-array-dimensions
   #:lazy-index-components
   #:with-lazy-arrays)

  ;; Evaluation
  (:export
   #:compute
   #:compute-asynchronously
   #:wait
   #:completedp
   #:make-unknown
   #:evaluator)

  ;; Core Operators
  (:export
   #:broadcast
   #:lazy
   #:lazy-multiple-value
   #:lazy-reshape
   #:lazy-fuse)

  ;; Reshapers
  (:export
   #:collapsing-reshaper
   #:peeling-reshaper
   #:permuting-reshaper)

  ;; Automatic Differentiation
  (:export
   #:differentiator)

  ;; Standard Library
  (:export
   #:lazy-overwrite
   #:lazy-reduce
   #:lazy-rearrange
   #:lazy-slice
   #:lazy-slices
   #:lazy-sort
   #:lazy-stack
   #:lazy-harmonize
   #:lazy-harmonize-list-of-arrays
   #:lazy-fuse-and-harmonize
   #:lazy-overwrite-and-harmonize)

  ;; Backends
  (:export
   #:*backend*
   #:backend
   #:backendp
   #:delete-backend
   #:with-temporary-backend
   #:make-reference-backend
   #:make-ir-backend
   #:make-native-backend))

(defpackage #:petalisp-user
  (:use #:common-lisp #:petalisp))
