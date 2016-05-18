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
     (:file "operators" :depends-on ("petalisp"))
     (:file "io" :depends-on ("petalisp"))
     (:file "cl-interop" :depends-on ("petalisp"))
     (:file "graphs" :depends-on ("petalisp"))))))

