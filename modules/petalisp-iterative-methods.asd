(defsystem :petalisp-iterative-methods
  :description "A collection of iterative methods, written in Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :depends-on ("asdf" "petalisp")

  :in-order-to ((test-op (test-op :petalisp-test-suite)))

  :serial t
  :components
  ((:file "iterative-methods")))
