(defsystem :petalisp-test-suite
  :description "Test suite for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :depends-on (:alexandria :petalisp :fiveam)
  :components
  ((:module "code"
    :components
    ((:module "test-suite"
      :serial t
      :components
      ((:module "examples"
        :components
                ((:file "jacobi")
                 (:file "red-black-gauss-seidel")
                 (:file "linear-algebra")))
       (:file "package")
       (:file "generators")
       (:file "code-statistics")
       (:file "test-suite"))))))
  :perform
  (test-op (o c) (symbol-call "PETALISP-TEST-SUITE" "RUN-TEST-SUITE")))
