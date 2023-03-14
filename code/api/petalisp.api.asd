(defsystem "petalisp.api"
  :description "A Convenient API for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "trivia"
   "split-sequence"
   "trivial-macroexpand-all"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.xmas-backend")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :components
  ((:file "differentiator" :depends-on ("lazy-reshape" "lazy-overwrite" "lazy" "lazy-reduce"))
   (:file "lazy-broadcast" :depends-on ("lazy-reshape"))
   (:file "lazy-change-shape" :depends-on ("shape-syntax"))
   (:file "lazy-drop-axes" :depends-on ("lazy-reshape"))
   (:file "lazy-index-components" :depends-on ("packages"))
   (:file "lazy" :depends-on ("lazy-broadcast"))
   (:file "lazy-multiple-value" :depends-on ("lazy-broadcast"))
   (:file "lazy-multireduce" :depends-on ("lazy-reduce"))
   (:file "lazy-overwrite" :depends-on ("packages"))
   (:file "lazy-reduce" :depends-on ("lazy-multiple-value" "lazy-reshape" "lazy-drop-axes" "lazy-stack"))
   (:file "lazy-reshape" :depends-on ("shape-syntax" "lazy-change-shape"))
   (:file "lazy-slice" :depends-on ("packages"))
   (:file "lazy-slices" :depends-on ("packages"))
   (:file "lazy-stack" :depends-on ("lazy-overwrite" "lazy-reshape"))
   (:file "network" :depends-on ("lazy" "lazy-reshape"))
   (:file "packages")
   (:file "reshapers" :depends-on ("lazy-reshape"))
   (:file "shape-syntax" :depends-on ("packages"))
   (:file "transform" :depends-on ("packages"))
   (:file "documentation"
    :depends-on
    ("differentiator"
     "lazy-broadcast"
     "lazy-drop-axes"
     "lazy-index-components"
     "lazy"
     "lazy-multiple-value"
     "lazy-multireduce"
     "lazy-overwrite"
     "lazy-reduce"
     "lazy-reshape"
     "reshapers"
     "lazy-slice"
     "lazy-slices"
     "lazy-stack"
     "network"
     "packages"
     "shape-syntax"
     "transform"))))
