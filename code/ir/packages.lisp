;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
   #:program-from-lazy-arrays

   ;; Structs
   #:program
   #:task
   #:buffer
   #:kernel
   #:stencil
   #:instruction
   #:iterating-instruction
   #:load-or-store-instruction
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
   #:load-or-store-instruction-p
   #:iref-instruction-p
   #:load-instruction-p
   #:store-instruction-p

   ;; Constructors
   #:make-program
   #:make-task
   #:make-kernel
   #:make-buffer
   #:make-stencil
   #:stencil-from-instruction

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
   #:map-stencil-instructions
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
   #:do-kernel-load-stencils
   #:do-kernel-load-instructions
   #:do-kernel-store-instructions
   #:do-stencil-instructions
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
   #:kernel-instruction-vector
   #:kernel-number-of-ienputs
   #:kernel-number-of-outputs
   #:kernel-number-of-loads
   #:kernel-number-of-stores
   #:kernel-cost
   #:kernel-task
   #:kernel-program
   #:kernel-number
   #:kernel-reuse-potential
   #:kernel-load-stencils
   #:kernel-store-stencils
   #:kernel-targets
   #:kernel-sources
   #:stencil-buffer
   #:stencil-input-rank
   #:stencil-output-rank
   #:stencil-output-mask
   #:stencil-scalings
   #:stencil-center
   #:stencil-instructions
   #:instruction-number
   #:instruction-inputs
   #:instruction-transformation
   #:instruction-number-of-values
   #:iref-instruction-transformation
   #:call-instruction-fnrecord
   #:call-instruction-function
   #:call-instruction-number-of-values
   #:call-instruction-inputs
   #:store-instruction-buffer
   #:store-instruction-transformation
   #:store-instruction-input
   #:load-instruction-buffer
   #:load-instruction-transformation

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

   ;; Partitioning
   #:layout
   #:layoutp
   #:layout-offset
   #:layout-strides
   #:layout-ntype
   #:layout-size
   #:layout-rank
   #:layout-ghost-layer-alist
   #:layout-allocation
   #:kernel-shard
   #:kernel-shard-p
   #:kernel-shard-kernel
   #:kernel-shard-iteration-space
   #:kernel-shard-targets
   #:kernel-shard-sources
   #:kernel-shard-more-important-p
   #:buffer-shard
   #:buffer-shard-p
   #:buffer-shard-buffer
   #:buffer-shard-parent
   #:buffer-shard-domain
   #:buffer-shard-shape
   #:buffer-shard-readers
   #:buffer-shard-writers
   #:buffer-shard-split
   #:buffer-shard-bits
   #:buffer-shard-layout
   #:buffer-shard-maxdepth
   #:buffer-shard-path
   #:buffer-shard-primogenitor
   #:split
   #:splitp
   #:split-parent
   #:split-axis
   #:split-position
   #:split-left-child
   #:split-right-child
   #:vicinity
   #:vicinityp
   #:vicinity-left-neighbors
   #:vicinity-right-neighbors
   #:compute-buffer-shard-vicinity
   #:partition-program

   ;; Miscellaneous
   #:*stencil-max-radius*
   #:compute-stencil-center
   #:make-buffer-like-array
   #:make-array-from-shape-and-ntype
   #:ensure-array-buffer-compatibility
   #:ensure-array-shape-ntype-compatibility
   #:check-ir
   #:compute-program-buffer-coloring
   #:reuse-optimizing-transformation))
