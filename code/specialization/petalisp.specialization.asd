(defsystem "petalisp.specialization"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("ucons")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "specialized-types")
   (:file "specialized-functions")
   (:file "specialize")))
