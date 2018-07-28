(defsystem :petalisp
  :description "Elegant High Performance Computing"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("agnostic-lizard"
   "alexandria"
   "bordeaux-threads"
   "lparallel"
   "iterate"
   "trivia"
   "trivial-arguments"
   "uiop")

  :in-order-to ((test-op (test-op :petalisp-test-suite)))

  :serial t
  :components
  ((:module "code"
    :components
    ((:file "packages")
     (:file "error-handling")

     (:module "utilities"
      :components
      ((:file "optimization")
       (:file "memoization")
       (:file "atomic-types")
       (:file "extended-euclid")
       (:file "function-lambda-lists")
       (:file "generic-funcallable-object")
       (:file "miscellaneous")
       (:file "queue")
       (:file "request")
       (:file "ucons")))

     (:module "type-inference"
      :components
      ((:file "inference")
       (:file "numbers")
       (:file "data-and-control-flow")))

     (:module "sets"
      :components
      ((:file "set-protocol")
       (:file "range-protocol")
       (:file "range-specialization")))

     (:module "transformations"
      :components
      ((:file "transformation")
       (:file "identity-transformation")
       (:file "invertible-transformation")
       (:file "hairy-transformation")
       (:file "constructors")))

     (:module "data-structures"
      :components
      ((:file "index-space")
       (:file "data-structure-method-combination")
       (:file "data-structure")
       (:file "strided-array-index-space")
       (:file "strided-array")
       (:file "strided-array-immediate")))

     (:module "kernel-creation"
      :components
      ((:file "blueprint")
       (:file "kernel")
       (:file "map-subtrees")
       (:file "map-subtree-fragments")
       (:file "build-kernel")
       (:file "kernelize")))

     (:module "backends"
      :components
      ((:file "backend")
       (:file "default-scheduler-mixin")
       (:file "compile-cache-mixin")
       (:file "reference-backend")
       (:file "testing-backend")
       (:file "common-lisp-backend")))

     (:file "api")))))
