(defsystem :petalisp
  :description "Elegant code generation for high-performance computing."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  (:agnostic-lizard
   :alexandria
   :closer-mop
   :fiveam
   :iterate
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
     (:module "utilities" :depends-on ("package")
      :components
      ((:file "generic")
       (:file "graphviz")
       (:file "introspection")
       (:file "iterate")
       (:file "macros")
       (:file "matrix" :depends-on ("macros" "testing"))
       (:file "miscellaneous" :depends-on ("macros" "testing"))
       (:file "testing")))
     (:file "petalisp" :depends-on ("utilities"))
     (:module "transformations" :depends-on ("petalisp")
      :components
      ((:file "identity-transformation")
       (:file "affine-transformation")
       (:file "classify-transformation" :depends-on ("identity-transformation" "affine-transformation"))))
     (:module "data-structures" :depends-on ("transformations")
      :components
      ((:file "range")
       (:file "strided-array-application" :depends-on ("strided-array"))
       (:file "strided-array-elaboration" :depends-on ("strided-array"))
       (:file "strided-array-fusion" :depends-on ("strided-array"))
       (:file "strided-array-index-space" :depends-on ("strided-array" "range"))
       (:file "strided-array")
       (:file "strided-array-reduction" :depends-on ("strided-array"))
       (:file "strided-array-reference" :depends-on ("strided-array"))))
     (:module "compiler" :depends-on ("data-structures")
      :components
      ())
     (:file "api" :depends-on ("data-structures"))))))

