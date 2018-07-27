(defsystem :petalisp-development
  :description "Developer utilities for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on ("petalisp"
               "cl-dot"
               "uiop")
  :perform
  (test-op (o c) (symbol-call "PETALISP/TEST-SUITE/TEST-SUITE" "RUN-TEST-SUITE"))

  :serial t
  :components
  ((:module "development"
    :components
    ((:file "graphviz")
     (file "visualization")))))
