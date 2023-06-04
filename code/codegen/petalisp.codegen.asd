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
   "trivia"
   "trivial-macroexpand-all")

  :serial t
  :components
  ((:file "packages")
   (:file "indexing")
   (:file "load-foreign-code")
   (:file "generic-functions")
   (:file "blueprint")
   (:file "bpinfo")
   (:file "compile-cache")
   (:file "lisp-compiler")
   (:file "lisp-interpreter")
   (:file "cpp-from-lisp")
   (:file "cpp-compiler")
   (:file "cuda-compiler")
   (:file "mixins")
   (:file "ir-backend")))
