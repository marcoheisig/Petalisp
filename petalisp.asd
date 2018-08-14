(defsystem :petalisp
  :description "Elegant High Performance Computing"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("agnostic-lizard"
   "alexandria"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "trivial-arguments")

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
       (:file "extended-euclid")
       (:file "identical")
       (:file "miscellaneous")
       (:file "symmetric-function")
       (:file "optimizing-constructor")
       (:file "generators")
       (:file "ucons")))

     (:module "type-inference"
      :components
      ((:file "atomic-types")
       (:file "function-lambda-lists")
       (:file "inference")
       (:file "numbers")
       (:file "data-and-control-flow")))

     (:module "sets"
      :components
      ((:file "set")
       (:file "empty-set")
       (:file "explicit-set")
       (:file "range")
       (:file "shape")))

     (:module "transformations"
      :components
      ((:file "transformation")
       (:file "identity-transformation")
       (:file "invertible-transformation")
       (:file "hairy-transformation")
       (:file "constructors")
       (:file "shape-transformations")))

     (:module "strided-arrays"
      :components
      ((:file "strided-array")
       (:file "immediate")
       (:file "application")
       (:file "reduction")
       (:file "fusion")
       (:file "reference")))

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
       (:module "reference-backend"
        :components ((:file "simple-immediate")
                     (:file "reference-backend")))
       (:module "native-backend"
        :components ((:file "scheduler")
                     (:file "compile-cache-mixin")
                     (:file "native-backend")))))

     (:file "api")))))
