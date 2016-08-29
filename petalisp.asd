(in-package :asdf-user)

(defsystem :petalisp
  :description "Elegant code generation for high-performance computing."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.1"
  :license "GPLv3"
  :depends-on (:alexandria :optima :fare-memoization)
  :components
  ((:module "code"
    :components
    ((:file "package")
     (:file "utilities" :depends-on ("package"))
     (:file "petalisp" :depends-on ("utilities"))
     (:file "transformation" :depends-on ("petalisp"))
     (:file "types" :depends-on ("petalisp"))
     (:file "graphviz" :depends-on ("petalisp"))
     (:module "strided-array" :depends-on ("petalisp")
      :components
      ((:file "strided-array")
       (:file "application" :depends-on ("strided-array"))
       (:file "broadcast" :depends-on ("strided-array"))
       (:file "difference" :depends-on ("strided-array"))
       (:file "fusion" :depends-on ("strided-array"))
       (:file "intersection" :depends-on ("strided-array"))
       (:file "print-object" :depends-on ("strided-array"))
       (:file "reduction" :depends-on ("strided-array"))
       (:file "reference" :depends-on ("strided-array"))
       (:file "repetition" :depends-on ("strided-array"))
       (:file "source" :depends-on ("strided-array"))
       (:file "transform" :depends-on ("strided-array"))))
     (:file "api" :depends-on ("strided-array"))))))

