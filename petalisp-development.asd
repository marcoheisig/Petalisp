(defsystem :petalisp-development
  :description "Developer utilities for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on ("petalisp"
               "cl-dot"
               "uiop")
  :in-order-to ((test-op (test-op :petalisp)))

  :serial t
  :components
  ((:module "code"
    :components
    ((:module "graphviz"
      :components
      ((:file "utilities")
       (:file "protocol")
       (:file "data-flow-graph")
       (:file "task-graph")
       (:file "view")))))))
