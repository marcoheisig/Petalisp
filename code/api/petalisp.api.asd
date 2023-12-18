(defsystem "petalisp.api"
  :description "A Convenient API for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "trivia"
   "split-sequence"
   "trivial-macroexpand-all"
   "petalisp.packages"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir"
   "petalisp.codegen"
   "petalisp.native-backend"
   "petalisp.graphviz")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "with-lazy-arrays")
   (:file "shape-syntax")
   (:file "transform")
   (:file "reshapers")
   (:file "broadcast")
   (:file "lazy-index-components")
   (:file "lazy")
   (:file "lazy-multiple-value")
   (:file "lazy-overwrite")
   (:file "lazy-stack")
   (:file "lazy-rearrange")
   (:file "harmonize")
   (:file "lazy-reduce")
   (:file "lazy-sort")
   (:file "differentiator")
   (:file "documentation")))
