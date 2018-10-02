;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(cl:defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
  (:shadow cl:set-difference alexandria:set-equal)
  (:export

   ;; Sets
   #:set-difference
   #:set-elements
   #:set-emptyp
   #:set-contains
   #:set-intersection
   #:set-intersectionp
   #:set-subsetp
   #:set-size
   #:set-union

   ;; Ranges
   #:range
   #:rangep
   #:unary-range-p
   #:split-range
   #:make-range
   #:range-start-step-end
   #:range-start
   #:range-step
   #:range-end

   ;; Shapes
   #:make-shape
   #:dimension
   #:ranges
   #:shape-difference-list
   #:shape-from-ranges
   #:enlarge-shape

   ;; Transformations
   #:transformation
   #:transformationp
   #:input-dimension
   #:output-dimension
   #:invertible-transformation-p
   #:canonicalize-transformation
   #:with-shape-accessors
   #:make-transformation
   #:make-identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation
   #:enlarge-transformation
   #:map-transformation-outputs

   ;; Strided Arrays
   #:strided-array
   #:element-type
   #:shape
   #:shapep
   #:size
   #:input
   #:inputs
   #:storage
   #:axis
   #:operator
   #:value-n
   #:refcount
   #:reduction-range
   #:application
   #:reduction
   #:fusion
   #:reference
   #:immediate
   #:immediatep
   #:scalar-immediate
   #:array-immediate
   #:range-immediate
   #:make-scalar-immediate
   #:make-array-immediate
   #:make-range-immediate

   ;; High-level API
   #:α
   #:alpha
   #:β
   #:beta
   #:reshape
   #:transform
   #:τ
   #:fuse
   #:fuse*
   #:compute
   #:schedule
   #:indices

   ;; Backends
   #:*backend*
   #:compute-on-backend
   #:schedule-on-backend
   #:compute-immediates
   #:backend
   #:asynchronous-backend
   #:delete-backend))
