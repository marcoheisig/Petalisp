(defsystem :petalisp-test-suite
  :description "Test suite for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :depends-on ("asdf"
               "petalisp"
               "petalisp-linear-algebra"
               "petalisp-iterative-methods"
               "fiveam")

  :perform
  (test-op (o c) (symbol-call "PETALISP-TEST-SUITE" "RUN-TEST-SUITE"))

  :serial t
  :components
  ((:file "packages")
   (:file "code-statistics")
   (:file "run")
   (:file "test-api")
   (:file "test-sets")
   (:file "test-iterative-methods")
   (:file "test-linear-algebra")))
