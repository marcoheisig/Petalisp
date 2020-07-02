(defsystem "petalisp.utilities"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "atomics"
   "trivia"
   "trivial-garbage")

  :serial t
  :components
  ((:file "packages")
   (:file "documentation")
   (:file "defalias")
   (:file "bitfield")
   (:file "identical")
   (:file "memoization")
   (:file "extended-euclid")
   (:file "float-bits")
   (:file "prime-factors")
   (:file "weak-set")
   (:file "with-collectors")))
