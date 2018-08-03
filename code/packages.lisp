;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
  (:shadow cl:set-difference alexandria:set-equal)
  (:export
   ;; Sets
   #:set-difference
   #:set-elements
   #:set-emptyp
   #:set-intersection
   #:set-intersectionp
   #:set-size
   #:set-union

   ;; Ranges
   #:range-start-step-end
   #:range-start
   #:range-step
   #:range-end
   #:make-range

   #:ranges
   #:canonicalize-index-space

   ;; Transformations
   #:canonicalize-transformation
   #:with-index-space-accessors
   #:make-transformation
   #:make-identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations
   #:collapsing-transformation

   ;; Data Structures
   #:canonicalize-data-structure
   #:data-structure
   #:element-type
   #:shape
   #:input
   #:inputs
   #:transformation
   #:storage-array
   #:application
   #:application-operator
   #:reduction
   #:reduction-binary-operator
   #:reduction-unary-operator
   #:reduction-order
   #:fusion
   #:reference
   #:immediate
   #:array-immediate
   #:strided-array-immediate

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
   #:size
   #:dimension

   ;; Backends
   #:backend
   #:compute-immediates
   #:compute-asynchronously
   #:compute-synchronously
   #:reference-backend
   #:common-lisp-backend
   #:testing-backend))

(cl:defpackage :petalisp-reference-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp))

(cl:defpackage :petalisp-native-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :alexandria :petalisp))

(cl:defpackage :petalisp-user
  (:shadowing-import-from :petalisp :set-difference)
  (:use :common-lisp :petalisp))
