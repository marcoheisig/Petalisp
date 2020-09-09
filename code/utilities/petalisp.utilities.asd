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
   (:file "deque")
   (:file "bitfield")
   (:file "identical")
   (:file "memoization")
   (:file "extended-euclid")
   (:file "prime-factors")
   (:file "weak-set")
   (:file "with-collectors")
   (:file "number-of-cpus")
   (:file "assocf")))
