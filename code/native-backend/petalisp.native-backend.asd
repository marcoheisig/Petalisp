(defsystem "petalisp.native-backend"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "atomics"
   "bordeaux-threads"
   "cffi"
   "lparallel"
   "trivia"
   "trivial-garbage"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir"
   "petalisp.codegen"
   "static-vectors"
   "typo")

  :serial t
  :components
  ((:file "packages")
   (:file "request")
   (:file "pin-current-thread")
   (:file "worker-pool")
   (:file "backend")
   (:file "scheduling")
   (:file "allocation")
   (:file "evaluator")
   (:file "compilation")))
