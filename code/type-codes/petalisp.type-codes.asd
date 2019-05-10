(defsystem "petalisp.type-codes"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on ("alexandria"
               "trivial-arguments")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "type-codes")
   (:file "function-lambda-lists")
   (:file "type-inference")
   (:module "type-inference-rules"
    :components
    ((:file "characters")
     (:file "data-and-control-flow")
     (:file "numbers")
     (:file "objects")
     (:file "symbols")))))
