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
   (:file "codegen-shared")
   (:file "codegen-cpp")
   (:file "codegen-cuda")
   (:file "program-codelet-vector")
   (:file "starpu-backend")
   (:file "dstate")
   (:file "cstate")
   (:file "execution")))
