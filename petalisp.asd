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
       (:file "broadcast" :depends-on ("strided-array"))
       (:file "intersection" :depends-on ("strided-array"))
       (:file "make-application" :depends-on ("strided-array"))
       (:file "make-fusion" :depends-on ("strided-array"))
       (:file "make-reduction" :depends-on ("strided-array"))
       (:file "make-repetition" :depends-on ("strided-array"))
       (:file "make-selection" :depends-on ("strided-array"))
       (:file "make-source" :depends-on ("strided-array"))
       (:file "make-target" :depends-on ("strided-array"))
       (:file "make-transformation" :depends-on ("strided-array"))
       (:file "print-object" :depends-on ("strided-array"))))))))

