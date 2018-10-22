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
   (:file "memory-pool")
   (:file "translation-unit")
   (:file "native-backend")
   (:file "ir-conversion")
   (:file "translation-unit-from-blueprint")
   (:file "lambda-expression-from-translation-unit")
   (:file "blueprint-compiler")
   (:file "execution")))
