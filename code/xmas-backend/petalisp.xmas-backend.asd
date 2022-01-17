(defsystem "petalisp.xmas-backend"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "trivial-garbage"
   "petalisp.utilities"
   "petalisp.type-inference"
   "petalisp.core"
   "petalisp.ir")

  :serial t
  :components
  ((:file "packages")
   (:file "memory-pool")
   (:file "xmas-backend")))
