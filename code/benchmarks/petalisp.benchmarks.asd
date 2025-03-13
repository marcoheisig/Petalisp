(defsystem "petalisp.benchmarks"
  :description "Measure the performance of various Petalisp operations"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "uiop"
   "petalisp")

  :serial t
  :components
  ((:file "packages")
   (:file "benchmark")
   (:file "dgemm")
   (:file "multigrid")))
