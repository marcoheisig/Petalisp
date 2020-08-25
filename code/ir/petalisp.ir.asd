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
   (:file "variables")
   (:file "ir")
   (:file "layout")
   (:file "layout-table")
   (:file "ir-checker")
   (:file "map-iteration-spaces")
   (:file "ir-conversion")
   (:file "blueprint")
   (:file "documentation")))
