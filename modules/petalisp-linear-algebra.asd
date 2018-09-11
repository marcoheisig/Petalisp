(defsystem :petalisp-linear-algebra
  :description "A collection of linear algebra subroutines, written in Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :depends-on ("asdf" "petalisp")

  :in-order-to ((test-op (test-op :petalisp-test-suite)))

  :serial t
  :components
  ((:file "linear-algebra")))
