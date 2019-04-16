(defsystem "petalisp.type-codes"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on ("alexandria" "trivia")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "float-bits")
   (:file "type-codes")
   (:file "caching")
   (:file "conversion")))
