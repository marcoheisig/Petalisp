(defsystem "petalisp.test-suite"
  :description "A comprehensive test suite for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("asdf"
   "petalisp"
   "petalisp-examples")

  :perform
  (test-op (o c) (symbol-call '#:petalisp.test-suite '#:run-petalisp-test-suite))

  :serial t
  :components
  ((:file "packages")
   (:file "test-suite")
   (:file "code-statistics")
   (:file "testing-backend")
   (:file "defgenerator")
   (:file "number-generators")
   (:file "petalisp-generators")
   (:file "run-petalisp-test-suite")
   (:file "api-tests")
   (:file "example-tests")
   (:file "hall-of-shame")))
