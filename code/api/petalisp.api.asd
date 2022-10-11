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
   (:file "lazy-allreduce" :depends-on ("lazy-reduce"))
   (:file "lazy-array-indices" :depends-on ("lazy-shape-indices"))
   (:file "lazy-array-interior" :depends-on ("shape-interior"))
   (:file "lazy-broadcast" :depends-on ("lazy-reshape"))
   (:file "lazy-drop-axes" :depends-on ("lazy-reshape"))
   (:file "lazy-flatten" :depends-on ("lazy-reshape"))
   (:file "lazy" :depends-on ("lazy-broadcast"))
   (:file "lazy-multiple-value" :depends-on ("lazy-broadcast"))
   (:file "lazy-overwrite" :depends-on ("packages"))
   (:file "lazy-reduce" :depends-on ("lazy-multiple-value" "lazy-reshape" "lazy-drop-axes" "lazy-stack"))
   (:file "lazy-reshape" :depends-on ("shape-syntax"))
   (:file "lazy-shape-indices" :depends-on ("packages"))
   (:file "lazy-slice" :depends-on ("packages"))
   (:file "lazy-slices" :depends-on ("packages"))
   (:file "lazy-stack" :depends-on ("lazy-overwrite" "lazy-reshape"))
   (:file "network" :depends-on ("lazy" "lazy-reshape"))
   (:file "packages")
   (:file "prepare" :depends-on ("packages"))
   (:file "shape-interior" :depends-on ("packages"))
   (:file "shape-syntax" :depends-on ("packages"))
   (:file "transform" :depends-on ("packages"))
   (:file "vectorize" :depends-on ("lazy-multiple-value"))
   (:file "documentation"
    :depends-on
    ("differentiator"
     "lazy-allreduce"
     "lazy-array-indices"
     "lazy-array-interior"
     "lazy-broadcast"
     "lazy-drop-axes"
     "lazy-flatten"
     "lazy"
     "lazy-multiple-value"
     "lazy-overwrite"
     "lazy-reduce"
     "lazy-reshape"
     "lazy-shape-indices"
     "lazy-slice"
     "lazy-slices"
     "lazy-stack"
     "network"
     "packages"
     "prepare"
     "shape-interior"
     "shape-syntax"
     "transform"
     "vectorize"))))
