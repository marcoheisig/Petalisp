(defsystem "petalisp.utilities"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on ()

  :serial t
  :components
  ((:file "packages")
   (:file "identical")
   (:file "memoization")
   (:file "extended-euclid")
   (:file "symmetric-function")
   (:file "define-class-predicate")
   (:file "defalias")))
