(defsystem :petalisp
  :description "Elegant code generation for high-performance computing."
  :author "Marco Heisig <marco.heisig@fau.de>"
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
  (test-op (o c)
           (format t "== Testing Petalisp ==~%")
           (uiop:symbol-call :petalisp '#:print-platform-information)
           (uiop:symbol-call :petalisp '#:print-system-statistics :petalisp)
           (uiop:symbol-call :petalisp '#:print-package-statistics :petalisp)
           (uiop:symbol-call '#:fiveam '#:run! (find-symbol "PETALISP" "PETALISP")))

  :components
  ((:module "code"
    :components
    ((:file "package")
     (:file "utilities"       :depends-on ("package"))
     (:file "petalisp"        :depends-on ("utilities"))
     (:file "linear-algebra"  :depends-on ("petalisp"))
     (:file "transformation"  :depends-on ("linear-algebra"))
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
     (:file "api" :depends-on ("strided-array"))))))

