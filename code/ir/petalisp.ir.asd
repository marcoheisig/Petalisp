(defsystem "petalisp.ir"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "petalisp.utilities"
   "petalisp.core")

  :serial t
  :components
  ((:file "packages")
   (:file "ir")
   (:file "compute-buffer-table")
   (:file "map-iteration-spaces")
   (:file "ir-conversion")
   (:file "blueprint")
   (:file "documentation")))
