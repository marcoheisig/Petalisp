(defsystem :petalisp-test-suite
  :description "Test suite for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :depends-on ("asdf"
               "petalisp"
               "fiveam")

  :perform
  (test-op (o c) (symbol-call "PETALISP-TEST-SUITE" "RUN-TEST-SUITE"))

  :serial t
  :components
  ((:module "examples"
    :components
    ((:file "jacobi")
     (:file "red-black-gauss-seidel")
     (:file "linear-algebra")))

   (:module "test-suite"
    :components
    ((:file "packages")
     (:file "code-statistics")
     (:file "test-suite")))))
