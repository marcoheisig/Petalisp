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
      ((:file "array-element-types")
       (:file "code-statistics")
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
     (:module "virtual-machines"
      :components
      ((:file "compile-cache-mixin")
       (:file "default-scheduler-mixin" :depends-on ("kernelize"))
       (:file "kernelize" :depends-on ("subgraph-kernels"))
       (:file "blueprint")
       (:file "reference-virtual-machine")
       (:file "testing-virtual-machine")
       (:file "subgraph-kernels" :depends-on ("blueprint"))
       (:module "common-lisp-virtual-machine"
        :depends-on ("default-scheduler-mixin"
                     "compile-cache-mixin")
        :components ((:file "common-lisp-virtual-machine")))))
     (:file "visualization")
     (:file "api")))))
