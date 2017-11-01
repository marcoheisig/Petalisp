(defsystem :petalisp
  :description "Elegant High Performance Computing"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  (:agnostic-lizard
   :alexandria
   :bordeaux-threads
   :closer-mop
   :fiveam
   :iterate
   :optima
   :the-cost-of-nothing
   :trivial-garbage
   :uiop)

  :perform (test-op (o c) (symbol-call "PETALISP" "RUN-TEST-SUITE"))

  :components
  ((:module "code"
    :serial t
    :components
    ((:file "package")
     (:module "utilities"
      :components
      ((:file "code-statistics")
       (:file "extended-euclid" :depends-on ("macros" "testing"))
       (:file "function-lambda-lists")
       (:file "fvector")
       (:file "generic-funcallable-objects")
       (:file "graphviz")
       (:file "iterate")
       (:file "macros")
       (:file "matrix" :depends-on ("macros" "testing"))
       (:file "memoization")
       (:file "miscellaneous" :depends-on ("macros" "testing"))
       (:file "queue")
       (:file "request")
       (:file "testing")
       (:file "ucons")))
     (:file "petalisp")
     (:module "transformations"
      :components
      ((:file "identity-transformation")
       (:file "affine-transformation" :depends-on ("identity-transformation"))
       (:file "classify-transformation" :depends-on ("affine-transformation"))))
     (:module "data-structures"
      :components
      ((:file "range")
       (:file "strided-array-index-space" :depends-on ("range"))
       (:file "strided-array" :depends-on ("strided-array-index-space"))
       (:file "strided-array-immediate" :depends-on ("strided-array"))))
     (:module "scheduler"
      :components
      ((:file "kernelize")
       (:file "recipe")
       (:file "scheduler" :depends-on ("kernelize"))))
     (:module "virtual-machines"
      :components
      ((:file "virtual-machine")
       (:file "common-lisp-virtual-machine" :depends-on ("virtual-machine"))))
     (:file "visualization" :depends-on ("scheduler" "virtual-machines"))
     (:file "api" :depends-on ("scheduler" "virtual-machines"))))))
