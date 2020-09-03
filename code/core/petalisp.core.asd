(defsystem "petalisp.core"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "split-sequence"
   "trivial-macroexpand-all"
   "trivial-garbage"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "ucons"
   "petalisp.utilities"
   "petalisp.type-inference")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "range")
   (:file "shape")
   (:file "transformation")
   (:file "transformation-constructors")
   (:file "lazy-array")
   (:file "lazy-reshape")
   (:file "lazy-fuse")
   (:file "lazy-map")
   (:file "substitute-array")
   (:file "backend")
   (:file "documentation")))
