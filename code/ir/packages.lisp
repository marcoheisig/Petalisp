;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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

   ;; Structs
   #:program
   #:task
   #:buffer
   #:kernel
   #:stencil
   #:instruction
   #:iterating-instruction
   #:call-instruction
   #:load-instruction
   #:store-instruction
   #:iref-instruction

   ;; Predicates
   #:programp
   #:taskp
   #:bufferp
   #:leaf-buffer-p
   #:root-buffer-p
   #:interior-buffer-p
   #:kernelp
   #:stencilp
   #:instructionp
   #:call-instruction-p
   #:iterating-instruction-p
   #:iref-instruction-p
   #:load-instruction-p
   #:store-instruction-p

   ;; Constructors
   #:make-program
   #:make-task
   #:make-kernel
   #:make-buffer
   #:make-stencil
   #:stencil-from-load-instruction

   ;; Mapping
   #:map-program-tasks
   #:map-program-kernels
   #:map-program-buffers
   #:map-task-successors
   #:map-task-predecessors
   #:map-task-kernels
   #:map-task-defined-buffers
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
   #:map-kernel-stencils
   #:map-kernel-instructions
   #:map-instruction-inputs
   #:map-program-buffer-groups

   ;; Do Macros
   #:do-program-tasks
   #:do-task-successors
   #:do-task-predecessors
   #:do-task-kernels
   #:do-task-defined-buffers
   #:do-program-buffers
   #:do-program-kernels
   #:do-buffer-inputs
   #:do-buffer-outputs
   #:do-buffer-load-instructions
   #:do-buffer-store-instructions
   #:do-kernel-inputs
   #:do-kernel-outputs
   #:do-kernel-stencils
   #:do-kernel-load-instructions
   #:do-kernel-store-instructions
   #:do-instruction-inputs
   #:do-kernel-instructions
   #:do-program-buffer-groups

   ;; Accessors
   #:program-initial-task
   #:program-final-task
   #:program-leaf-alist
   #:program-root-buffers
   #:program-task-vector
   #:program-number-of-buffers
   #:program-number-of-kernels
   #:program-number-of-tasks
   #:program-buffer
   #:program-kernel
   #:task-program
   #:task-number
   #:buffer-shape
   #:buffer-size
   #:buffer-ntype
   #:buffer-depth
   #:buffer-storage
   #:buffer-task
   #:buffer-program
   #:buffer-bits
   #:buffer-number
   #:buffer-number-of-inputs
   #:buffer-number-of-outputs
   #:buffer-number-of-loads
   #:buffer-number-of-stores
   #:buffer-reuse-potential
   #:kernel-iteration-space
   #:kernel-blueprint
   #:kernel-instruction-vector
   #:kernel-number-of-inputs
   #:kernel-number-of-outputs
   #:kernel-number-of-loads
   #:kernel-number-of-stores
   #:kernel-cost
   #:kernel-task
   #:kernel-program
   #:kernel-number
   #:kernel-reuse-potential
   #:kernel-stencils
   #:stencil-buffer
   #:stencil-input-rank
   #:stencil-output-rank
   #:stencil-output-mask
   #:stencil-scalings
   #:stencil-center
   #:stencil-load-instructions
   #:instruction-number
   #:instruction-inputs
   #:instruction-transformation
   #:call-instruction-operator
   #:store-instruction-buffer
   #:load-instruction-buffer

   ;; Devices, Cores, and Memory
   #:device
   #:device-name
   #:device-memory
   #:device-cores
   #:host-device
   #:core
   #:core-name
   #:core-memory
   #:memory
   #:memory-name
   #:memory-parent
   #:memory-children
   #:memory-cores
   #:memory-size
   #:memory-granularity
   #:memory-latency
   #:memory-bandwidth
   #:memory-parent-bandwidth

   ;; Miscellaneous
   #:*stencil-max-radius*
   #:compute-stencil-center
   #:make-buffer-like-array
   #:ensure-array-buffer-compatibility
   #:make-ir-backend
   #:check-ir
   #:interpret-kernel
   #:translate-blueprint
   #:compute-program-buffer-coloring
   #:reuse-optimizing-transformation))
