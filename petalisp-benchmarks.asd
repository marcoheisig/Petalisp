(defsystem :petalisp-benchmarks
  :description "Benchmarks for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :encoding :utf-8
  :class :package-inferred-system
  :depends-on ("petalisp/benchmarks/benchmarks")
  :perform
  (test-op (o c) (symbol-call "PETALISP/BENCHMARKS/BENCHMARKS" "BENCHMARK-ALL")))
