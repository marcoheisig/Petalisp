;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :common-lisp-user)

(defpackage :petalisp-internals
  (:documentation
   "The internal methods and classes of Petalisp. This package exports all
    functionality required for testing, hacking new virtual machines or
    toying around with graph nodes and index spaces.")
  (:use :closer-common-lisp :alexandria :bordeaux-threads :trivial-garbage)
  ;; shadow CL:INTERSECTION and CL:UNION with generic methods
  (:shadow #:intersection #:union)
  ;; portable OS interface
  (:import-from :uiop #:run-program #:with-temporary-file #:native-namestring)
  ;; benchmark utilities
  (:import-from :the-cost-of-nothing #:bench #:nbench #:benchmark)
  ;; everyone needs a code walker
  (:import-from :agnostic-lizard
                #:macroexpand-all
                #:walk-form
                #:metaenv-function-like-entries
                #:metaenv-variable-like-entries
                #:metaenv-blocks
                #:metaenv-tags
                #:metaenv-ensure-name-from-environment
                #:metaenv-ensure-names-from-environment)
  ;; LOOP no more
  (:import-from :iterate #:display-iterate-clauses
                #:defsynonym #:dsetq #:declare-variables
                #:defmacro-clause #:defmacro-driver #:defclause-sequence
                #:initially #:after-each #:finally #:finally-protected
                #:else #:if-first-time #:first-iteration-p #:first-time-p
                #:finish #:leave #:next-iteration #:next #:terminate
                #:repeat #:for #:as #:generate #:generating #:in
                #:sum #:summing #:multiply #:multiplying
                #:maximize #:minimize #:maximizing #:minimizing #:counting
                #:always #:never #:thereis #:finding #:collect #:collecting
                #:with #:while #:until #:adjoining #:nconcing #:appending
                #:nunioning #:unioning #:reducing #:accumulate #:accumulating)
  ;; The internal API
  (:export
   #:affine-transformation #:affine-transformation?
   #:application #:application?
   #:atomic-array-element-type-specifier
   #:binary-operator
   #:binary-operator-metadata
   #:blueprint
   #:broadcast
   #:common-broadcast-space
   #:common-lisp-virtual-machine #:common-lisp-virtual-machine?
   #:compile-cache
   #:compile-cache-mixin #:compile-cache-mixin?
   #:complete
   #:composition
   #:compute
   #:compute-operator-metadata
   #:corresponding-immediate
   #:data-structure #:data-structure?
   #:default-scheduler-mixin #:default-scheduler-mixin?
   #:define-class
   #:define-symbol-pool
   #:dequeue
   #:difference
   #:dimension
   #:element-type
   #:enlarge-index-space
   #:enlarge-transformation
   #:enqueue
   #:ensure-sequence
   #:equal?
   #:extended-euclid
   #:from-storage
   #:from-storage-transformation
   #:funcall-form
   #:function-arity
   #:function-lambda-list
   #:fuse
   #:fuse*
   #:fuse-recursively
   #:fusion #:fusion?
   #:generate
   #:generator
   #:generic-binary-funcall
   #:generic-unary-funcall
   #:graphviz
   #:identical
   #:identity-matrix?
   #:identity-transformation #:identity-transformation?
   #:immediate #:immediate?
   #:index-space #:index-space?
   #:index-symbol
   #:index-symbol-list
   #:input-constraints
   #:input-dimension
   #:inputs
   #:intersection #:intersection?
   #:inverse
   #:invertible?
   #:iterate
   #:iteration-space
   #:iteration-space-normalization
   #:kernel #:kernel?
   #:kernelize
   #:kernels
   #:lambda-list-arity
   #:linear-operator
   #:make-application
   #:make-fusion
   #:make-immediate
   #:make-immediate!
   #:make-queue
   #:make-reduction
   #:make-reference
   #:make-request
   #:map-ulist
   #:matrix-identity?
   #:matrix-inverse
   #:matrix-m
   #:matrix-n
   #:matrix-product
   #:matrix-sum
   #:matrix #:matrix?
   #:memory-pool
   #:operator
   #:operator-metadata #:operator-metadata?
   #:order
   #:output-dimension
   #:queue?
   #:range #:range?
   #:range-end
   #:range-start
   #:range-step
   #:range-symbol
   #:range-symbol-list
   #:ranges
   #:reduction #:reduction?
   #:reference #:reference?
   #:reference-virtual-machine #:reference-virtual-machine?
   #:request?
   #:result-type
   #:scaled-permutation-matrix #:scaled-permutation-matrix?
   #:schedule
   #:scheduler-queue
   #:scheduler-thread
   #:shallow-copy
   #:size
   #:source-symbol
   #:source-symbol-list
   #:sources
   #:spaces-to-fuse
   #:spm-column-indices
   #:spm-m
   #:spm-n
   #:spm-values
   #:storage
   #:strided-array-application #:strided-array-application?
   #:strided-array-fusion #:strided-array-fusion?
   #:strided-array-immediate #:strided-array-immediate?
   #:strided-array-index-space #:strided-array-index-space?
   #:strided-array-reduction #:strided-array-reduction?
   #:strided-array-reference #:strided-array-reference?
   #:strided-array #:strided-array?
   #:subdivision
   #:subspace?
   #:target
   #:target-symbol
   #:target-symbol-list
   #:testing-virtual-machine #:testing-virtual-machine?
   #:to-storage
   #:transformation #:transformation?
   #:translation-vector
   #:ulist-deep-copy
   #:unary-operator
   #:unary-operator-metadata
   #:unary-range?
   #:virtual-machine #:virtual-machine?
   #:*virtual-machine*
   #:virtual-machines
   #:vm/bind-memory
   #:vm/compile
   #:vm/compute
   #:vm/evaluate
   #:vm/execute
   #:vm/free-memory
   #:vm/schedule
   #:wait
   #:α #:β #:σ #:σ* #:τ #:->))

(defpackage :petalisp
  (:documentation "The Petalisp API.")
  (:import-from
   :petalisp-internals . #1=
   (#:α
    #:β
    #:->
    #:τ
    #:σ
    #:σ*
    #:schedule
    #:compute
    #:*virtual-machine*
    #:reference-virtual-machine
    #:common-lisp-virtual-machine
    #:testing-virtual-machine))
  (:export . #1#))
