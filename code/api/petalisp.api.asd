(defsystem "petalisp.api"
  :description "A Convenient API for Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "trivia"
   "petalisp.utilities"
   "petalisp.core"
   "petalisp.reference-backend"
   "petalisp.ir-backend"
   "petalisp.native-backend")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :components
  (;; Define the aliases after the documentation has been established, such
   ;; that the documentation is already present and can be copied, too.
   (:file "aliases" :depends-on ("documentation"))
   (:file "alpha" :depends-on ("broadcast"))
   (:file "beta" :depends-on ("alpha" "reshape" "drop-axes" "stack"))
   (:file "broadcast" :depends-on ("reshape"))
   (:file "collapse" :depends-on ("reshape"))
   (:file "differentiator" :depends-on ("reshape" "fuse" "alpha" "beta"))
   (:file "drop-axes" :depends-on ("reshape"))
   (:file "flatten" :depends-on ("reshape"))
   (:file "fuse" :depends-on ("packages"))
   (:file "interior" :depends-on ("packages"))
   (:file "network" :depends-on ("alpha" "reshape"))
   (:file "packages")
   (:file "reshape" :depends-on ("packages"))
   (:file "slice" :depends-on ("packages"))
   (:file "slices" :depends-on ("packages"))
   (:file "stack" :depends-on ("fuse" "reshape"))
   (:file "vectorize" :depends-on ("alpha"))
   (:file "documentation"
    :depends-on
    ("alpha"
     "beta"
     "broadcast"
     "collapse"
     "differentiator"
     "drop-axes"
     "flatten"
     "fuse"
     "interior"
     "network"
     "packages"
     "reshape"
     "drop-axes"
     "slice"
     "slices"
     "stack"
     "vectorize"))))
