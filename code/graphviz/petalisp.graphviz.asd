(defsystem "petalisp.graphviz"
  :description "Graphviz visualization of Petalisp data structures."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "uiop"
   "trivial-features"
   "petalisp.packages"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.ir"
   "petalisp.native-backend"
   "cl-dot")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "protocol")
   (:file "petalisp")
   (:file "data-flow-graph")
   (:file "ir")
   (:file "partitioning")
   (:file "class-diagram")
   (:file "scheduling")
   (:file "view")))
