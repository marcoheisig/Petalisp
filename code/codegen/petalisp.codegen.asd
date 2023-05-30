(defsystem "petalisp.codegen"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "cffi"
   "ucons"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir"
   "trivia")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "generic-functions")
   (:file "blueprint")
   (:file "compile-cache")
   (:file "lisp-compiler")
   (:file "lisp-interpreter")
   (:file "cpp-compiler")
   (:file "cuda-compiler")
   (:file "mixins")
   (:file "ir-backend")))
