(defsystem :petalisp-test-suite
  :description "Test suite for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :encoding :utf-8
  :class :package-inferred-system
  :depends-on ("petalisp/test-suite/test-suite")
  :perform
  (test-op (o c) (symbol-call "PETALISP/TEST-SUITE/TEST-SUITE" "RUN-TEST-SUITE")))
