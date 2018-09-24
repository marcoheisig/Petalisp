(defsystem :petalisp-core
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("agnostic-lizard"
   "alexandria"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "trivial-arguments")

  :serial t
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

   (:module "backends"
    :components
    ((:file "backend")))))
