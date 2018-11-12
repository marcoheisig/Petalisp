(defsystem :petalisp-api
  :description "A Convenient API for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "trivia"
   "named-readtables"
   "petalisp-core"
   "petalisp-reference-backend"
   "petalisp-ir-backend"
   "petalisp-native-backend")

  :in-order-to ((test-op (test-op :petalisp-development)))

  :serial t
  :components
  ((:file "packages")
   (:file "api")))
