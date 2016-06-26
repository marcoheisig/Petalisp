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
     (:file "index-spaces" :depends-on ("error-handling"))
     (:file "operator-database" :depends-on ("error-handling"))
     (:file "petalisp" :depends-on ("index-spaces" "operator-database"))
     (:file "graphviz" :depends-on ("petalisp"))))))

