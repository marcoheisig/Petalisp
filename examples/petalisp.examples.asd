(defsystem "petalisp.examples"
  :description "A collection of Petalisp usage examples."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on ("petalisp")
  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "iterative-methods")
   (:file "linear-algebra")))
