(defsystem "petalisp.core"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "agnostic-lizard"
   "split-sequence"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "ucons"
   "petalisp.utilities"
   "petalisp.type-codes"
   "petalisp.specialization")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "range")
   (:file "shape")
   (:file "transformation")
   (:file "transformation-constructors")
   (:file "lazy-array")
   (:file "broadcasting")
   (:file "lazy-array-constructors")
   (:file "backend")
   (:file "documentation")))
