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
   #:range #:rangep
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
   #:transformation #:transformationp
   #:invertible-transformation-p
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
   #:shape #:shapep
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
   #:immediate #:immediatep
   #:scalar-immediate
   #:array-immediate
   #:range-immediate
   #:make-scalar-immediate
   #:make-array-immediate
   #:make-range-immediate

   ;; High-level API
   #:*backend*
   #:α #:alpha
   #:β #:beta
   #:reshape
   #:transform
   #:τ
   #:fuse
   #:fuse*
   #:compute
   #:schedule
   #:indices

   ;; Intermediate Representation
   #:ir-from-strided-arrays
   #:make-buffer
   #:make-kernel
   #:compute-buffer-table
   #:compute-kernels
   #:ir-node
   #:kernel #:kernelp
   #:body
   #:buffer #:bufferp
   #:pstore
   #:pref
   #:preduce
   #:pcall

   ;; Backends
   #:compute-on-backend
   #:schedule-on-backend
   #:compute-immediates
   #:backend

   #:reference-backend
   #:ir-backend
   #:native-backend
   ))

(cl:defpackage :petalisp-reference-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:reference-backend))

(cl:defpackage :petalisp-ir-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:ir-backend))

(cl:defpackage :petalisp-native-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:native-backend))
