;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.core
  (:use #:common-lisp)

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

   ;; Ranges
   #:range
   #:rangep
   #:empty-range
   #:empty-range-p
   #:non-empty-range
   #:non-empty-range-p
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
   #:subrangep
   #:fuse-ranges

   ;; Shapes
   #:shape
   #:shapep
   #:make-shape
   #:empty-shape-p
   #:shape-rank
   #:shape-range
   #:shape-ranges
   #:shape-dimensions
   #:shape-size
   #:shape=
   #:shape<
   #:shape-difference-list
   #:shape-intersection
   #:shape-intersectionp
   #:map-shape
   #:shape-contains
   #:shrink-shape
   #:enlarge-shape
   #:subdivide-arrays
   #:subdivide-shapes
   #:subshapep
   #:fuse-shapes
   #:shape-table
   #:shape-table-p
   #:make-shape-table
   #:shape-table-value
   #:remove-shape-table-entry
   #:clear-shape-table
   #:array-shape
   #:array-value
   #:shape-designator-shape

   ;; Transformations
   #:transformation
   #:transformationp
   #:transform-sequence
   #:transform-shape
   #:transform-axis
   #:transform-lazy-array
   #:identity-transformation-p
   #:transformation-input-rank
   #:transformation-output-rank
   #:transformation-input-mask
   #:transformation-output-mask
   #:transformation-offsets
   #:transformation-scalings
   #:transformation-invertiblep
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation=
   #:transformation-similar
   #:compose-transformations
   #:collapsing-transformation
   #:normalizing-transformation
   #:enlarge-transformation
   #:map-transformation-inputs
   #:map-transformation-outputs

   ;; Lazy Arrays
   #:*lazy-array-lock*
   #:lazy-array
   #:lazy-array-p
   #:lazy-array-shape
   #:lazy-array-ntype
   #:lazy-array-depth
   #:lazy-array-refcount
   #:lazy-array-delayed-action
   #:lazy-array-element-type
   #:lazy-array-rank
   #:lazy-array-size
   #:lazy-array-ranges
   #:lazy-array-inputs
   #:lazy-array
   #:lazy-map
   #:lazy-multiple-value-map
   #:lazy-reshape
   #:lazy-ref
   #:lazy-fuse
   #:lazy-collapse
   #:make-unknown
   #:lazy-unknowns
   #:empty-lazy-array
   #:empty-lazy-arrays
   #:move-axis-to-front
   #:lazy-array-from-array
   #:lazy-array-from-scalar
   #:lazy-array-from-range
   #:copy-lazy-arrays
   #:substitute-lazy-arrays
   #:substitute-lazy-array
   #:substitute-delayed-action

   ;; Delayed Actions
   #:delayed-action
   #:delayed-action-p
   #:delayed-action-inputs
   #:delayed-map
   #:delayed-map-p
   #:delayed-map-operator
   #:delayed-map-inputs
   #:delayed-map-number-of-values
   #:delayed-multiple-value-map
   #:delayed-multiple-value-map-p
   #:delayed-multiple-value-map-operator
   #:delayed-multiple-value-map-inputs
   #:delayed-multiple-value-map-ntypes
   #:delayed-nth-value
   #:delayed-nth-value-p
   #:delayed-nth-value-number
   #:delayed-nth-value-input
   #:delayed-reshape
   #:delayed-reshape-p
   #:delayed-reshape-transformation
   #:delayed-reshape-input
   #:delayed-fuse
   #:delayed-fuse-p
   #:delayed-fuse-inputs
   #:delayed-range
   #:delayed-range-p
   #:make-delayed-array
   #:delayed-array
   #:delayed-array-p
   #:delayed-array-storage
   #:delayed-unknown
   #:delayed-unknown-p
   #:delayed-nop
   #:delayed-nop-p
   #:delayed-wait
   #:delayed-wait-p
   #:delayed-wait-request
   #:delayed-wait-delayed-action
   #:delayed-failure
   #:delayed-failure-p
   #:delayed-failure-condition

   ;; Request
   #:request
   #:make-request
   #:requestp
   #:request-backend
   #:request-lazy-arrays
   #:request-wait
   #:request-finish
   #:request-finishedp

   ;; Backend
   #:*backend*
   #:with-backend
   #:backend
   #:delete-backend
   #:backend-compute
   #:backend-schedule
   #:backend-evaluator
   #:compute
   #:compute-list-of-arrays
   #:schedule
   #:wait
   #:evaluator

   ;; Reference Backend
   #:make-reference-backend))
