(defsystem :petalisp-reference-backend
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("petalisp-core")

  :serial t
  :components
  ((:file "packages")
   (:file "intermediate-result")
   (:file "reference-backend")))
