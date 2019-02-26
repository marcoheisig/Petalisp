;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.core
  (:use :closer-common-lisp :alexandria)
  (:shadow cl:set-difference alexandria:set-equal)
  (:export

   ;; Sets
   #:set-for-each
   #:set-difference
   #:set-elements
   #:set-emptyp
   #:set-equal
   #:set-contains
   #:set-intersection
   #:set-intersectionp
   #:set-subsetp
   #:set-size
   #:set-union

   ;; Ranges
   #:range
   #:rangep
   #:size-one-range-p
   #:split-range
   #:make-range
   #:range-start-step-end
   #:range-start
   #:range-step
   #:range-end

   ;; Shapes
   #:~
   #:shape
   #:shapep
   #:make-shape
   #:rank
   #:ranges
   #:shape-difference-list
   #:enlarge-shape
   #:shrink-shape
   #:broadcast-shapes

   ;; Transformations
   #:τ
   #:transformation
   #:transformationp
   #:transform
   #:transform-axis
   #:identity-transformation-p
   #:input-rank
   #:output-rank
   #:invertible-transformation-p
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
   #:coerce-to-lazy-array
   #:total-size
   #:element-type
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
   #:broadcast-arrays
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
