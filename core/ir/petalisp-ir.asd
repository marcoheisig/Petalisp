(defsystem :petalisp-ir
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("petalisp-core")

  :serial t
  :components
  ((:file "packages")
   (:file "ir")
   (:file "compute-buffer-table")
   (:file "compute-iteration-spaces")
   (:file "ir-conversion")
   (:file "normalize-ir")))
