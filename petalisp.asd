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
    ((:file "setup")
     (:file "index-spaces" :depends-on ("setup"))
     (:file "petalisp" :depends-on ("index-spaces"))
     (:file "io" :depends-on ("petalisp"))
     (:file "cl-integration" :depends-on ("petalisp"))
     (:file "graphviz" :depends-on ("petalisp"))))))

