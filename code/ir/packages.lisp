;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.ir
  (:use
   #:common-lisp
   #:alexandria
   #:petalisp.core)
  (:shadowing-import-from :petalisp.core #:set-difference #:set-equal)
  (:import-from #:petalisp.utilities #:document-variable #:document-function)
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
   #:reduce-instruction

   ;; Predicates
   #:bufferp
   #:kernelp
   #:reduction-kernel-p
   #:instructionp
   #:call-instruction-p
   #:iterating-instruction-p
   #:iref-instruction-p
   #:load-instruction-p
   #:store-instruction-p
   #:reduce-instruction-p

   ;; Constructors
   #:make-kernel
   #:make-buffer

   ;; Mapping
   #:map-buffer-inputs
   #:map-buffer-outputs
   #:map-kernel-store-instructions
   #:map-kernel-load-instructions
   #:map-kernel-inputs
   #:map-kernel-outputs
   #:map-instruction-inputs
   #:map-instructions

   ;; Accessors
   #:buffer-shape
   #:buffer-type-code
   #:buffer-inputs
   #:buffer-outputs
   #:buffer-executedp
   #:buffer-reusablep
   #:buffer-storage
   #:kernel-iteration-space
   #:kernel-reduction-range
   #:kernel-loads
   #:kernel-stores
   #:kernel-executedp
   #:kernel-blueprint
   #:kernel-buffers
   #:kernel-highest-instruction-number
   #:kernel-number-of-loads
   #:kernel-number-of-stores
   #:instruction-number
   #:instruction-inputs
   #:instruction-transformation
   #:call-instruction-operator
   #:store-instruction-buffer
   #:load-instruction-buffer
   #:reduce-instruction-operator

   ;; Miscellaneous
   #:assign-instruction-numbers
   #:parse-kernel-blueprint))
