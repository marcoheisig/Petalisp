;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp-ir
  (:shadowing-import-from :petalisp #:set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export

   ;; Classes
   #:statement
   #:buffer
   #:kernel
   #:simple-kernel
   #:reduction-kernel

   ;; Generic Constructors
   #:make-statement
   #:make-kernel
   #:make-simple-kernel
   #:make-reduction-kernel
   #:make-buffer

   ;; Accessors
   #:operator
   #:loads
   #:stores
   #:shape
   #:element-type
   #:inputs
   #:outputs
   #:iteration-space
   #:body
   #:reduction-range
   #:reduction-stores

   ;; IR Conversion
   #:ir-from-strided-arrays
   #:compute-buffer-table
   #:compute-kernels
   #:reduction-value-symbol))
