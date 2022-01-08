(defsystem "petalisp.ir"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "ucons"
   "priority-queue"
   "petalisp.utilities"
   "petalisp.core")

  :serial t
  :components
  ((:file "packages")
   (:file "device")
   (:file "ir")
   (:file "ir-checker")
   (:file "ir-conversion")
   (:file "blueprint")
   (:file "kernel-interpreter")
   (:file "kernel-compiler")
   (:file "ir-backend")
   (:file "documentation")))
