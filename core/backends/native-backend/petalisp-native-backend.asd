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
   (:file "form-builder")
   (:file "blueprint-compiler")
   (:file "memory-pool")
   (:file "native-backend")
   (:file "ir-conversion")
   (:file "execution")))
