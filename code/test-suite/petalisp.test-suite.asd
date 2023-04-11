(defsystem "petalisp.test-suite"
  :description "A comprehensive test suite for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("asdf"
   "bordeaux-threads"
   "closer-mop"
   "petalisp.examples"
   "petalisp.xmas-backend")

  :perform
  (test-op (o c) (symbol-call '#:petalisp.test-suite '#:run-petalisp-test-suite))

  :serial t
  :components
  ((:file "packages")
   (:file "test-suite")
   (:file "testing-backend")
   (:file "code-statistics")
   (:file "defgenerator")
   (:file "generators")
   (:file "run-petalisp-test-suite")
   (:file "petalisp.core")
   (:file "petalisp.examples")))
