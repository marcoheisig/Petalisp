(defsystem "petalisp.scheduler"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("petalisp.core"
   "petalisp.ir"
   "atomics")

  :serial t
  :components
  ((:file "packages")
   (:file "work-stealing-deque")
   (:file "slice")
   (:file "task")
   (:file "scheduler")))
