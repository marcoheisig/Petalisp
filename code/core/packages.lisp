;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
   #:empty-shape
   #:empty-shape-p
   #:non-empty-shape
   #:non-empty-shape-p
   #:shape-rank
   #:shape-range
   #:shape-ranges
   #:shape-dimensions
   #:shape-size
   #:shape-equal
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

   ;; Transformations
   #:τ
   #:transformation
   #:transformationp
   #:transform
   #:transform-axis
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
   #:transformation-equal
   #:transformation-similar
   #:compose-transformations
   #:collapsing-transformation
   #:normalizing-transformation
   #:enlarge-transformation
   #:map-transformation-inputs
   #:map-transformation-outputs

   ;; Lazy Arrays
   #:lazy-array-p
   #:empty-array-p
   #:immediatep
   #:lazy-array
   #:array-shape
   #:parameter
   #:optional-parameter
   #:optional-parameter-value
   #:replace-lazy-array
   #:total-size
   #:element-type
   #:element-ntype
   #:rank
   #:lazy-array-input
   #:lazy-array-inputs
   #:lazy-multiple-value-ref-value-n
   #:lazy-map-operator
   #:lazy-map-number-of-values
   #:lazy-array-refcount
   #:lazy-array-depth
   #:lazy-array-shape
   #:lazy-array
   #:lazy-map
   #:array-immediate-storage
   #:lazy-multiple-value-map
   #:lazy-multiple-value-ref
   #:lazy-reshape
   #:lazy-fuse
   #:immediate
   #:non-immediate
   #:empty-array
   #:empty-arrays
   #:non-empty-array
   #:non-empty-immediate
   #:non-empty-non-immediate
   #:array-immediate
   #:range-immediate
   #:move-axis-to-front
   #:empty-array
   #:make-array-immediate
   #:make-range-immediate
   #:broadcast
   #:broadcast-arrays
   #:broadcast-list-of-arrays
   #:copy-arrays
   #:substitute-arrays

   ;; Backend
   #:*backend*
   #:make-reference-backend
   #:compute-on-backend
   #:schedule-on-backend
   #:compile-network-on-backend
   #:compute-immediates
   #:lisp-datum-from-immediate
   #:backend
   #:delete-backend
   #:compute
   #:schedule))
