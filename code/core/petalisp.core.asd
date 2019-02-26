(defsystem "petalisp.core"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "simplified-types"
   "restricted-functions"
   "ucons"
   "petalisp.utilities")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")

   ;; Sets
   (:file "set")
   (:file "empty-set")
   (:file "explicit-set")
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
   (:file "broadcasting")
   (:file "lazy-array")
   (:file "lazy-array-constructors")

   ;; The rest
   (:file "backend")
   (:file "documentation")))
