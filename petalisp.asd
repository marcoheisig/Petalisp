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

  :perform (test-op (o c) (uiop:symbol-call "PETALISP" "RUN-TEST-SUITE"))

  :components
  ((:module "code"
    :components
    ((:file "package")
     (:module "utilities" :depends-on ("package")
      :components
      ((:file "code-statistics")
       (:file "extended-euclid" :depends-on ("macros" "testing"))
       (:file "function-lambda-lists")
       (:file "fvector")
       (:file "generic-funcallable-objects")
       (:file "graphviz")
       (:file "hcons")
       (:file "iterate")
       (:file "macros")
       (:file "matrix" :depends-on ("macros" "testing"))
       (:file "memoization")
       (:file "miscellaneous" :depends-on ("macros" "testing"))
       (:file "queue")
       (:file "request")
       (:file "testing")))
     (:file "petalisp" :depends-on ("utilities"))
     (:module "transformations" :depends-on ("petalisp")
      :components
      ((:file "identity-transformation")
       (:file "affine-transformation" :depends-on ("identity-transformation"))
       (:file "classify-transformation" :depends-on ("affine-transformation"))))
     (:module "data-structures" :depends-on ("transformations")
      :components
      ((:file "range")
       (:file "strided-array-index-space" :depends-on ("range"))
       (:file "strided-array" :depends-on ("strided-array-index-space"))
       (:file "strided-array-immediate" :depends-on ("strided-array"))
       (:file "visualization" :depends-on ("strided-array-immediate"))))
     (:module "evaluator" :depends-on ("data-structures")
      :components
      ((:file "compiler" :depends-on ("kernel"))
       (:file "kernelize" :depends-on ("kernel"))
       (:file "kernel" :depends-on ("recipe"))
       (:file "memory-pool")
       (:file "recipe")
       (:file "scheduler" :depends-on ("kernelize" "memory-pool"))))
     (:file "api" :depends-on ("evaluator"))))))
