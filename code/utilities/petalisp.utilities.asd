(defsystem "petalisp.utilities"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "atomics"
   "bordeaux-threads"
   "queues.priority-queue"
   "trivia"
   "trivial-garbage")

  :serial t
  :components
  ((:file "packages")
   (:file "documentation")
   (:file "cqueue")
   (:file "extended-euclid")
   (:file "powers-of-two")
   (:file "prime-factors")
   (:file "with-collectors")
   (:file "number-of-cpus")
   (:file "graph-coloring")
   (:file "karmarkar-karp")
   (:file "scheduling")
   (:file "with-pinned-objects")))
