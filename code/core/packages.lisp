;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.core
  (:use #:common-lisp)

  (:import-from
   #:petalisp.utilities
   #:document-compiler-macro
   #:document-function
   #:document-method-combination
   #:document-setf-expander
   #:document-structure
   #:document-type
   #:document-variable)

  (:export

   ;; Ranges
   #:range
   #:rangep
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
   #:subdivide
   #:subshapep

   ;; Transformations
   #:τ
   #:transformation
   #:transformationp
   #:transform
   #:transform-axis
   #:identity-transformation-p
   #:transformation-input-rank
   #:transformation-output-rank
   #:transformation-invertiblep
   #:make-transformation
   #:identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation
   #:normalizing-transformation
   #:enlarge-transformation
   #:map-transformation-outputs

   ;; Lazy Arrays
   #:lazy-array-p
   #:empty-array-p
   #:immediatep
   #:reusablep
   #:lazy-array
   #:parameter
   #:optional-parameter
   #:optional-parameter-value
   #:replace-lazy-array
   #:total-size
   #:element-type
   #:element-ntype
   #:rank
   #:input
   #:inputs
   #:value-n
   #:operator
   #:storage
   #:refcount
   #:lazy-array
   #:lazy-map
   #:lazy-reduce
   #:lazy-rehape
   #:lazy-fuse
   #:immediate
   #:non-immediate
   #:empty-array
   #:non-empty-array
   #:non-empty-immediate
   #:non-empty-non-immediate
   #:array-immediate
   #:range-immediate
   #:move-axis-to-front
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
   #:copy-arrays
   #:substitute-arrays
   #:α
   #:α*
   #:β

   ;; Network
   #:network
   #:network-parameters
   #:network-outputs
   #:make-network
   #:call-network
   #:differentiator

   ;; Backend
   #:*backend*
   #:compute-on-backend
   #:schedule-on-backend
   #:compile-network-on-backend
   #:compute-immediates
   #:lisp-datum-from-immediate
   #:backend
   #:asynchronous-backend
   #:delete-backend
   #:compute
   #:schedule

   ;; Devices
   #:device
   #:devicep
   #:device-name
   #:device-number-of-workers
   #:device-memory-size
   #:compile-blueprint
   #:allocate-buffer
   #:deallocate-buffer
   #:move-buffer))
