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
     (:file "index-spaces" :depends-on ("package"))
     (:file "petalisp" :depends-on ("index-spaces"))
     (:file "graphviz" :depends-on ("petalisp"))))))

