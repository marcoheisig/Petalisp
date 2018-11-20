;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp-ir
  (:use :closer-common-lisp :alexandria :petalisp-core)
  (:shadowing-import-from :petalisp-core #:set-difference #:set-equal)
  (:export

   ;; Generic Functions
   #:make-buffer
   #:make-kernel
   #:rotate-buffer
   #:rotate-kernel
   #:map-instruction-inputs

   ;; Classes
   #:instruction
   #:buffer
   #:kernel
   #:instruction
   #:iterating-instruction
   #:call-instruction
   #:load-instruction
   #:store-instruction
   #:iref-instruction
   #:reduce-instruction

   ;; Accessors
   #:buffer-shape
   #:element-type
   #:inputs
   #:outputs
   #:iteration-space
   #:loads
   #:stores
   #:reduction-stores
   #:instruction-number
   #:transformation
   #:operator
   #:arguments
   #:value

   ;; Utilities
   #:reduction-kernel-p
   #:map-instructions
   #:map-buffers
   #:highest-instruction-number
   #:update-instruction-numbers
   #:kernel-buffers
   #:kernel-reduce-instructions

   ;; IR Conversion
   #:ir-from-strided-arrays
   #:compute-buffer-table
   #:compute-kernels
   #:reduction-value-symbol
   #:normalize-ir
   #:blueprint))
