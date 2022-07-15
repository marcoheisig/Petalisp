(defsystem "petalisp.starpu-backend"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "atomics"
   "bordeaux-threads"
   "cl-starpu"
   "trivia"
   "trivial-garbage"
   "petalisp.utilities"
   "petalisp.type-inference"
   "petalisp.core"
   "petalisp.ir"
   "split-sequence")

  :serial t
  :components
  ((:file "packages")
   (:file "blueprint-codelet")
   (:file "starpu-backend")))
