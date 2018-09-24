(defsystem :petalisp
  :description "Elegant High Performance Computing"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("petalisp-core"
   "petalisp-reference-backend"
   "petalisp-ir-backend"
   "petalisp-native-backend")

  :in-order-to ((test-op (test-op :petalisp-development)))

  :serial t
  :components
  ((:module "core"
    :components
    ((:file "api")))))
