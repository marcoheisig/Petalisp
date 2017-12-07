(defsystem :petalisp
  :description "Elegant High Performance Computing"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  (:agnostic-lizard
   :alexandria
   :bordeaux-threads
   :closer-mop
   :iterate
   :the-cost-of-nothing
   :trivial-garbage
   :uiop)

  :in-order-to ((test-op (test-op :petalisp-test-suite)))

  :components
  ((:module "code"
    :serial t
    :components
    ((:file "package")
     (:module "utilities"
      :components
      ((:file "array-element-types")
       (:file "code-statistics")
       (:file "extended-euclid" :depends-on ("macros"))
       (:file "function-lambda-lists")
       (:file "fvector")
       (:file "generic-funcallable-objects")
       (:file "graphviz")
       (:file "iterate")
       (:file "macros")
       (:file "matrix" :depends-on ("macros"))
       (:file "memoization")
       (:file "miscellaneous" :depends-on ("macros"))
       (:file "queue")
       (:file "request")
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
     (:file "kernelize")
     (:module "virtual-machines"
      :components
      ((:file "common-lisp-virtual-machine" :depends-on ("default-scheduler-mixin" "compile-cache-mixin"))
       (:file "compile-cache-mixin")
       (:file "default-scheduler-mixin")
       (:file "reference-virtual-machine")
       (:file "testing-virtual-machine")))
     (:file "visualization")
     (:file "api")))))

(defsystem :petalisp-test-suite
  :description "Test suite for the parallel programming library Petalisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"
  :depends-on (:alexandria :petalisp :fiveam)
  :components
  ((:module "code"
    :components
    ((:module "test-suite"
      :serial t
      :components
      ((:file "generators")
       (:file "test-suite"))))))
  :perform (test-op (o c)
                    (symbol-call "PETALISP-TEST-SUITE" "RUN-TEST-SUITE")))
