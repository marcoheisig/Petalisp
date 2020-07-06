;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.ir
  (:use
   #:common-lisp
   #:petalisp.core)

  (:import-from
   #:petalisp.utilities
   #:document-variable
   #:document-function)

  (:export
   ;; IR Conversion
   #:ir-from-lazy-arrays

   ;; IR Normalization
   #:normalize-ir

   ;; Structs
   #:instruction
   #:buffer
   #:kernel
   #:instruction
   #:iterating-instruction
   #:call-instruction
   #:load-instruction
   #:store-instruction
   #:iref-instruction

   ;; Predicates
   #:bufferp
   #:kernelp
   #:instructionp
   #:call-instruction-p
   #:iterating-instruction-p
   #:iref-instruction-p
   #:load-instruction-p
   #:store-instruction-p

   ;; Constructors
   #:make-kernel
   #:make-buffer

   ;; Mapping
   #:map-buffers-and-kernels
   #:map-kernels
   #:map-buffers
   #:map-buffer-inputs
   #:map-buffer-outputs
   #:map-buffer-load-instructions
   #:map-buffer-store-instructions
   #:map-kernel-store-instructions
   #:map-kernel-load-instructions
   #:map-kernel-inputs
   #:map-kernel-outputs
   #:map-instruction-inputs
   #:map-instructions

   ;; Accessors
   #:buffer-shape
   #:buffer-ntype
   #:buffer-readers
   #:buffer-writers
   #:buffer-executedp
   #:buffer-reusablep
   #:buffer-storage
   #:kernel-iteration-space
   #:kernel-executedp
   #:kernel-blueprint
   #:kernel-buffers
   #:kernel-highest-instruction-number
   #:kernel-number-of-loads
   #:kernel-number-of-stores
   #:kernel-cost
   #:instruction-number
   #:instruction-inputs
   #:instruction-transformation
   #:call-instruction-operator
   #:store-instruction-buffer
   #:load-instruction-buffer

   ;; Miscellaneous
   #:assign-instruction-numbers
   #:parse-kernel-blueprint))
