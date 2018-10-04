(defsystem :petalisp-development
  :description "Developer utilities for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("asdf"
   "uiop"
   "petalisp"
   "petalisp-ir"
   "petalisp-reference-backend"
   "petalisp-ir-backend"
   "petalisp-native-backend"
   "petalisp-linear-algebra"
   "petalisp-iterative-methods"
   "the-cost-of-nothing"
   "cl-dot"
   "1am")

  :perform
  (test-op (o c) (symbol-call "PETALISP-DEVELOPMENT" "RUN-TEST-SUITE"))

  :serial t
  :components
  ((:file "packages")
   (:file "code-statistics")

   (:module "graphviz"
    :components
    ((:file "utilities")
     (:file "protocol")
     (:file "strided-arrays")
     (:file "ir")
     (:file "view")))

   (:module "generators"
    :components
    ((:file "defgenerator")
     (:file "number-generators")
     (:file "array-generators")
     (:file "range-generators")
     (:file "shape-generators")))

   (:module "test-suite"
    :components
    ((:file "utilities")
     (:file "run")
     (:file "test-api")
     (:file "test-sets")
     (:file "test-iterative-methods")
     (:file "test-linear-algebra")))))
