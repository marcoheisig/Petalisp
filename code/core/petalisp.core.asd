(defsystem "petalisp.core"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "lparallel"
   "trivia"
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
   (:file "lazy-ref")
   (:file "lazy-fuse")
   (:file "lazy-map")
   (:file "evaluation")
   (:file "reference-backend")
   (:file "substitute-lazy-arrays")))
