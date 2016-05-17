(in-package :asdf-user)

(defsystem #:petalisp
  :description "Run Common Lisp on really big Machines."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.1"
  :license "GPLv3"
  :depends-on (:optima :closer-mop)
  :components
  ((:module "src"
    :components
    ((:file "packages")
     (:file "graphs" :depends-on ("packages"))
     (:file "io" :depends-on ("packages"))
     (:file "petalisp" :depends-on ("io" "graphs"))
     (:file "cl-interop" :depends-on ("petalisp"))))))

