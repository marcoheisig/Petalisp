(defsystem :petalisp-development
  :description "Developer utilities for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("asdf"
   "uiop"
   "petalisp"
   "petalisp-examples"
   "the-cost-of-nothing"
   "cl-dot")

  :perform
  (test-op (o c) (symbol-call "PETALISP-DEVELOPMENT" "RUN-PETALISP-TEST-SUITE"))

  :serial t
  :components
  ((:file "packages")

   (:module "graphviz"
    :components
    ((:file "utilities")
     (:file "protocol")
     (:file "petalisp")
     (:file "data-flow-graph")
     (:file "ir")
     (:file "view")))

   (:module "generators"
    :components
    ((:file "defgenerator")
     (:file "numbers")
     (:file "petalisp-primitives")
     (:file "petalisp-programs")))

   (:module "test-suite"
    :components
    ((:file "test-suite")
     (:file "code-statistics")
     (:file "testing-backend")
     (:file "run-petalisp-test-suite")
     (:file "api")
     (:file "sets")
     (:file "iterative-methods")
     (:file "linear-algebra")
     (:file "hall-of-shame")))))
