(defsystem "petalisp.type-inference"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "trivia"
   "trivial-arguments")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "auxiliary-types")
   (:file "function-lambda-lists")
   (:file "ntype")
   (:file "ntype-operations")
   (:file "conditions")
   (:file "special-functions")
   (:file "define-rule")
   (:file "define-instruction")
   (:file "specialize")
   (:module "common-lisp"
    :components
    ((:file "auxiliary")
     (:file "data-and-control-flow")
     (:file "type-checks")
     (:file "casts")
     (:file "types-and-classes")
     (:file "add")
     (:file "sub")
     (:file "mul")
     (:file "div")
     (:file "cmpeq")
     (:file "cmpneq")
     (:file "cmpx")
     (:file "min")
     (:file "max")
     ))))
