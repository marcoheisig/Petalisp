(defsystem "petalisp.test-suite"
  :description "A comprehensive test suite for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("asdf"
   "closer-mop"
   "petalisp"
   "petalisp.examples"
   "petalisp.graphviz")

  :perform
  (test-op (o c) (symbol-call '#:petalisp.test-suite '#:run-petalisp-test-suite))

  :serial t
  :components
  ((:file "packages")
   (:file "test-suite")
   (:file "code-statistics")
   (:file "testing-backend")
   (:file "defgenerator")
   (:file "generators")
   (:file "run-petalisp-test-suite")
   (:file "petalisp.type-codes")
   (:file "petalisp.core")
   (:file "petalisp.examples")))
