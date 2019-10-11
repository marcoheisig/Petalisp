;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.core
  (:use #:common-lisp)
  (:import-from #:petalisp.utilities #:document-variable #:document-function)
  (:export

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
   #:~
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
   #:subdivision

   ;; Transformations
   #:τ
   #:transformation
   #:transformationp
   #:transform
   #:transform-axis
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
   #:lazy-array-p
   #:empty-array-p
   #:immediatep
   #:reusablep
   #:coerce-to-lazy-array
   #:total-size
   #:element-type
   #:rank
   #:ntype
   #:input
   #:inputs
   #:value-n
   #:operator
   #:storage
   #:refcount
   #:lazy-array
   #:immediate
   #:non-immediate
   #:empty-array
   #:non-empty-array
   #:non-empty-immediate
   #:non-empty-non-immediate
   #:array-immediate
   #:range-immediate
   #:application
   #:reduction
   #:fusion
   #:reference
   #:make-reference
   #:reshape
   #:fuse
   #:fuse*
   #:empty-array
   #:make-array-immediate
   #:make-range-immediate
   #:indices
   #:broadcast
   #:broadcast-arrays
   #:broadcast-list-of-arrays
   #:α
   #:β

   ;; Backends
   #:*backend*
   #:compute-on-backend
   #:schedule-on-backend
   #:compute-immediates
   #:lisp-datum-from-immediate
   #:backend
   #:asynchronous-backend
   #:overwrite-instance
   #:delete-backend
   #:compute
   #:schedule))
