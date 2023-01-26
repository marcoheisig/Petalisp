(defsystem :petalisp
  :description "Elegant High Performance Computing"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :in-order-to ((test-op (test-op "petalisp.test-suite")))
  :depends-on
  ("petalisp.api"
   "petalisp.test-suite"))
