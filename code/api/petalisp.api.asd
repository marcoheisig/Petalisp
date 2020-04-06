(defsystem "petalisp.api"
  :description "A Convenient API for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "trivia"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.reference-backend"
   "petalisp.ir-backend"
   "petalisp.native-backend")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "reshape")
   (:file "collapse")
   (:file "fuse")
   (:file "drop-axes")
   (:file "flatten")
   (:file "slice")
   (:file "slices")
   (:file "stack")
   (:file "beta")
   (:file "vectorize")
   (:file "differentiator")
   (:file "network")
   (:file "documentation")
   (:file "aliases")))
