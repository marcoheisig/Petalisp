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
     (:file "classes" :depends-on ("package"))
     (:file "types" :depends-on ("package"))
     (:file "functions" :depends-on ("classes"))
     (:file "petalisp" :depends-on ("functions"))
     (:file "graphviz" :depends-on ("classes"))
     (:module "strided-array" :depends-on ("classes" "functions")
      :components
      ((:file "strided-array")
       (:file "generic-apply" :depends-on ("strided-array"))
       (:file "generic-broadcast" :depends-on ("strided-array"))
       (:file "generic-dimension" :depends-on ("strided-array"))
       (:file "generic-equalp" :depends-on ("strided-array"))
       (:file "generic-index-space" :depends-on ("strided-array"))
       (:file "generic-fuse" :depends-on ("strided-array"))
       (:file "generic-intersect" :depends-on ("strided-array"))
       (:file "generic-invert" :depends-on ("strided-array"))
       (:file "generic-reduce" :depends-on ("strided-array"))
       (:file "generic-repeat" :depends-on ("strided-array"))
       (:file "generic-select" :depends-on ("strided-array"))
       (:file "generic-size" :depends-on ("strided-array"))
       (:file "generic-source" :depends-on ("strided-array"))
       (:file "generic-target" :depends-on ("strided-array"))
       (:file "generic-transform" :depends-on ("strided-array"))
       (:file "print-object" :depends-on ("strided-array"))))))))

