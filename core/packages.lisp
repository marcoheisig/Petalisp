;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(cl:defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
  (:shadow cl:set-difference alexandria:set-equal)
  (:export

   ;; Sets
   #:set-for-each
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
   #:shape
   #:shapep
   #:make-shape
   #:rank
   #:ranges
   #:shape-difference-list
   #:enlarge-shape

   ;; Transformations
   #:transformation
   #:transformationp
   #:identity-transformation-p
   #:input-rank
   #:output-rank
   #:invertible-transformation-p
   #:canonicalize-transformation
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation
   #:enlarge-transformation
   #:map-transformation-outputs

   ;; Strided Arrays
   #:strided-array
   #:coerce-to-strided-array
   #:lisp-datum-from-immediate
   #:element-type
   #:total-size
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
   #:a
   #:β
   #:b
   #:reshape
   #:transform
   #:τ
   #:fuse
   #:fuse*
   #:compute
   #:schedule
   #:indices
   #:petalisp-readtable

   ;; Backends
   #:*backend*
   #:compute-on-backend
   #:schedule-on-backend
   #:compute-immediates
   #:backend
   #:asynchronous-backend
   #:overwrite-instance
   #:delete-backend))
