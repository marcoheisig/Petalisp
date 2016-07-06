(in-package :asdf-user)

(defsystem :petalisp
  :description "Elegant code generation for high-performance computing."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.1"
  :license "GPLv3"
  :depends-on (:optima)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "error-handling")
     (:file "petalisp" :depends-on ("error-handling"))
     (:file "operator-database" :depends-on ("error-handling"))
     (:file "graphviz" :depends-on ("petalisp"))
     (:module "strided-array" :depends-on ("petalisp")
      :components
      ((:file "strided-array")
       (:file "generic-apply" :depends-on ("strided-array"))
       (:file "generic-broadcast" :depends-on ("strided-array"))
       (:file "generic-dimension" :depends-on ("strided-array"))
       (:file "generic-equalp" :depends-on ("strided-array"))
       (:file "generic-fuse" :depends-on ("strided-array"))
       (:file "generic-input" :depends-on ("strided-array"))
       (:file "generic-intersect" :depends-on ("strided-array"))
       (:file "generic-invert" :depends-on ("strided-array"))
       (:file "generic-reduce" :depends-on ("strided-array"))
       (:file "generic-repeat" :depends-on ("strided-array"))
       (:file "generic-select" :depends-on ("strided-array"))
       (:file "generic-size" :depends-on ("strided-array"))
       (:file "generic-transform" :depends-on ("strided-array"))
       (:file "print-object" :depends-on ("strided-array"))))))))

