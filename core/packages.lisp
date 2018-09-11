;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
  (:shadow cl:set-difference alexandria:set-equal)
  (:export
   ;; Generators
   #:make-generator
   #:generate-instance

   ;; Sets
   #:set-difference
   #:set-elements
   #:set-emptyp
   #:set-contains
   #:set-intersection
   #:set-intersectionp
   #:set-size
   #:set-union

   ;; Ranges
   #:range
   #:rangep
   #:unary-range-p
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

   ;; Transformations
   #:transformation
   #:canonicalize-transformation
   #:with-shape-accessors
   #:make-transformation
   #:make-identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation

   ;; Strided Arrays
   #:strided-array
   #:strided-array
   #:element-type
   #:shape
   #:size
   #:input
   #:inputs
   #:storage
   #:axis
   #:operator
   #:value-n
   #:application
   #:reduction
   #:fusion
   #:reference
   #:immediate
   #:scalar-immediate
   #:array-immediate
   #:range-immediate

   ;; High-level API
   #:*backend*
   #:α
   #:β
   #:reshape
   #:transform
   #:τ
   #:fuse
   #:fuse*
   #:compute
   #:schedule
   #:indices

   ;; Backends
   #:compute-on-backend
   #:schedule-on-backend
   #:compute-immediates
   #:backend

   #:reference-backend
   #:native-backend
   ))

(cl:defpackage :petalisp-reference-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:reference-backend))

(cl:defpackage :petalisp-native-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:native-backend))

(cl:defpackage :petalisp-user
  (:shadowing-import-from :petalisp :set-difference)
  (:use :common-lisp :petalisp))
