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

   ;; Ranges and Shapes
   (:file "range")
   (:file "shape")

   ;; Transformations
   (:file "transformation")
   (:file "identity-transformation")
   (:file "invertible-transformation")
   (:file "hairy-transformation")
   (:file "transformation-constructors")
   (:file "shape-transformations")

   ;; Lazy arrays
   (:file "lazy-array")
   (:file "broadcasting")
   (:file "lazy-array-constructors")

   ;; The rest
   (:file "backend")
   (:file "documentation")))
