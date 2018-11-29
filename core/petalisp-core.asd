(defsystem :petalisp-core
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "trivial-arguments")

  :in-order-to ((test-op (test-op :petalisp-development)))

  :serial t
  :components
  ((:file "packages")
   (:file "error-handling")

   (:module "utilities"
    :components
    ((:file "memoization")
     (:file "extended-euclid")
     (:file "identical")
     (:file "miscellaneous")
     (:file "symmetric-function")
     (:file "optimizing-constructor")
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
     (:file "make-transformation")
     (:file "shape-transformations")))

   (:module "strided-arrays"
    :components
    ((:file "strided-array")
     (:file "immediate")
     (:file "reference")
     (:file "application")
     (:file "reduction")
     (:file "fusion")))

   (:module "backends"
    :components
    ((:file "backend")))))
