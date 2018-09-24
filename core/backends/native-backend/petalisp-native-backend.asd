(defsystem :petalisp-native-backend
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("petalisp-core"
   "petalisp-ir")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "memory-pool")
   (:file "native-backend")
   (:file "ir-conversion")
   (:file "blueprint")
   (:file "blueprint-compiler")
   (:file "scheduler")))
