(in-package :asdf-user)

(defsystem :petalisp
  :description "Elegant code generation for high-performance computing."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.1"
  :license "GPLv3"
  :depends-on (:alexandria :optima)
  :components
  ((:module "code"
    :components
    ((:file "package")
     (:file "utilities" :depends-on ("package"))
     (:file "petalisp" :depends-on ("utilities"))
     (:file "types" :depends-on ("petalisp"))
     (:file "graphviz" :depends-on ("petalisp"))
     (:file "api" :depends-on ("petalisp"))
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
       (:file "repetition" :depends-on ("strided-array"))
       (:file "selection" :depends-on ("strided-array"))
       (:file "source" :depends-on ("strided-array"))
       (:file "target" :depends-on ("strided-array"))
       (:file "transformation" :depends-on ("strided-array"))))))))

