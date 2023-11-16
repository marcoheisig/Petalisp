(defsystem "petalisp.packages"
  :description "The PETALISP and PETALISP-USER packages."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :components
  ((:file "packages")))
