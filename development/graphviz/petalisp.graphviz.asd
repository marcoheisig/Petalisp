(defsystem "petalisp.graphviz"
  :description "Graphviz visualization of Petalisp data structures."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("uiop"
   "petalisp"
   "cl-dot")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "protocol")
   (:file "petalisp")
   (:file "data-flow-graph")
   (:file "ir")
   (:file "view")))
