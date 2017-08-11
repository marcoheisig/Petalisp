(defsystem :petalisp
  :description "Elegant code generation for high-performance computing."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.1"
  :license "AGPLv3"

  :depends-on
  (:alexandria
   :anaphora
   :closer-mop
   :fare-memoization
   :fiveam
   :optima
   :trivial-garbage)

  :perform
  (test-op (o s)
           (uiop:symbol-call '#:petalisp '#:run-test-suite))

  :components
  ((:module "code"
    :components
    ((:file "package")
     (:file "utilities"       :depends-on ("package"))
     (:file "petalisp"        :depends-on ("utilities"))
     (:file "transformation"  :depends-on ("petalisp"))
     (:file "database"        :depends-on ("petalisp"))
     (:file "graphviz"        :depends-on ("petalisp"))
     (:file "compute"         :depends-on ("petalisp"))
     (:module "strided-array" :depends-on ("petalisp")
      :components
      ((:file "strided-array")
       (:file "application"  :depends-on ("strided-array"))
       (:file "broadcast"    :depends-on ("strided-array"))
       (:file "difference"   :depends-on ("strided-array"))
       (:file "fusion"       :depends-on ("strided-array"))
       (:file "intersection" :depends-on ("strided-array"))
       (:file "print-object" :depends-on ("strided-array"))
       (:file "reduction"    :depends-on ("strided-array"))
       (:file "reference"    :depends-on ("strided-array"))
       (:file "repetition"   :depends-on ("strided-array"))
       (:file "transform"    :depends-on ("strided-array"))))
     (:file "api" :depends-on ("strided-array"))
     (:file "test-suite" :depends-on ("api"))))))

