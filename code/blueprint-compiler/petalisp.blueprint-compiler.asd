(defsystem "petalisp.blueprint-compiler"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "petalisp.utilities"
   "petalisp.core")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "basic-block")
   (:file "translation-unit")
   (:file "blueprint-compiler")))
