;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
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

   ;; Set operations
   #:make-range
   #:range-start
   #:range-step
   #:range-end
   #:range-size
   #:ranges
   #:canonicalize-index-space

   ;; Transformations
   #:canonicalize-transformation
   #:with-index-space-accessors
   #:make-transformation
   #:make-identity-transformation
   #:invert-transformation
   #:transformation-equal
   #:compose-transformations))

(cl:defpackage :petalisp-native-backend
  (:use :closer-common-lisp :petalisp))

(cl:defpackage :petalisp-user
  (:use :common-lisp :petalisp))
