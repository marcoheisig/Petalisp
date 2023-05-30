(defsystem "petalisp.xmas-backend"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "atomics"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "trivial-garbage"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir"
   "petalisp.codegen"
   "typo")

  :serial t
  :components
  ((:file "packages")
   (:file "memory-pool")
   (:file "worker-pool")
   (:file "xmas-backend")))
