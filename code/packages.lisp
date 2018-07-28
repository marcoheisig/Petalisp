;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
  (:shadow cl:set-difference alexandria:set-equal)
  (:export
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
   #:*backend*
   #:reference-backend
   #:common-lisp-backend
   #:testing-backend

   ;; Set protocol
   #:set-difference
   #:set-elements
   #:set-emptyp
   #:set-intersection
   #:set-intersectionp
   #:set-size
   #:set-union

   ;; Range protocol
   #:range-start
   #:range-step
   #:range-end
   #:make-range

   #:ranges
   #:canonicalize-index-space

   ;; Transformation protocol
   #:canonicalize-transformation
   #:with-index-space-accessors
   #:make-transformation
   #:make-identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations))

(cl:defpackage :petalisp-native-backend
  (:shadowing-import-from :petalisp :set-difference)
  (:use :closer-common-lisp :petalisp))

(cl:defpackage :petalisp-user
  (:shadowing-import-from :petalisp :set-difference)
  (:use :common-lisp :petalisp))
