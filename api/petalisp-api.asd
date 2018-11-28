(defsystem :petalisp-api
  :description "A Convenient API for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("agnostic-lizard"
   "alexandria"
   "closer-mop"
   "trivia"
   "named-readtables"
   "split-sequence"
   "petalisp-core"
   "petalisp-reference-backend"
   "petalisp-ir-backend"
   "petalisp-native-backend")

  :in-order-to ((test-op (test-op :petalisp-development)))

  :serial t
  :components
  ((:file "packages")
   (:file "transformations")
   (:file "shapes")
   (:file "api")))
