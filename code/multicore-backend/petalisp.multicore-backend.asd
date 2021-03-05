(defsystem "petalisp.multicore-backend"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "atomics"
   "trivia"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir")

  :serial t
  :components
  ((:file "packages")
   (:file "worker-pool")
   (:file "allocation-table")
   (:file "multicore-backend")))
