(defsystem :petalisp-development
  :description "Developer utilities for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :class :package-inferred-system
  :depends-on ("petalisp/development/all"
               "cl-dot")
  :perform
  (test-op (o c) (symbol-call "PETALISP/TEST-SUITE/TEST-SUITE" "RUN-TEST-SUITE")))
