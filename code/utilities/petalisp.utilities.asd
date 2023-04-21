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
   (:file "queue")
   (:file "extended-euclid")
   (:file "powers-of-two")
   (:file "prime-factors")
   (:file "with-collectors")
   (:file "number-of-cpus")
   (:file "topological-sort")
   (:file "graph-coloring")
   (:file "karmarkar-karp")
   (:file "scheduling" :depends-on ("queue"))))
