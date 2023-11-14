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
   "petalisp.ir"
   "petalisp.codegen"
   "petalisp.native-backend")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :components
  ((:file "differentiator" :depends-on ("lazy-overwrite" "lazy" "lazy-reduce"))
   (:file "broadcast")
   (:file "lazy-rearrange" :depends-on ("shape-syntax"))
   (:file "lazy-drop-axes")
   (:file "lazy-harmonize" :depends-on ("lazy" "lazy-overwrite"))
   (:file "lazy-index-components" :depends-on ("packages"))
   (:file "lazy" :depends-on ("broadcast"))
   (:file "lazy-multiple-value" :depends-on ("broadcast"))
   (:file "lazy-multireduce" :depends-on ("lazy-reduce"))
   (:file "lazy-overwrite" :depends-on ("packages"))
   (:file "lazy-reduce" :depends-on ("lazy-multiple-value" "lazy-drop-axes" "lazy-stack"))
   (:file "lazy-slice" :depends-on ("packages"))
   (:file "lazy-slices" :depends-on ("packages"))
   (:file "lazy-sort" :depends-on ("lazy-multiple-value"))
   (:file "lazy-stack" :depends-on ("lazy-overwrite"))
   (:file "network" :depends-on ("lazy"))
   (:file "packages")
   (:file "reshapers")
   (:file "shape-syntax" :depends-on ("packages"))
   (:file "transform" :depends-on ("packages"))
   (:file "with-lazy-arrays" :depends-on ("packages"))
   (:file "with-backend" :depends-on ("packages"))
   (:file "documentation"
    :depends-on
    ("differentiator"
     "broadcast"
     "lazy-drop-axes"
     "lazy-harmonize"
     "lazy-index-components"
     "lazy"
     "lazy-multiple-value"
     "lazy-multireduce"
     "lazy-overwrite"
     "lazy-reduce"
     "reshapers"
     "lazy-slice"
     "lazy-slices"
     "lazy-sort"
     "lazy-stack"
     "network"
     "packages"
     "shape-syntax"
     "transform"
     "with-lazy-arrays"
     "with-backend"))))
