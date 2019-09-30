(defsystem "petalisp.native-backend"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "lparallel"
   "trivia"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir"
   "petalisp.scheduler"
   "petalisp.blueprint-compiler")

  :serial t
  :components
  ((:file "packages")
   (:file "worker-pool")
   (:file "memory-pool")
   (:file "native-backend")
   (:file "execution")))
