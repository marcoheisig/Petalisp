(defsystem "petalisp.examples"
  :description "A collection of Petalisp usage examples."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("petalisp.api"
   "numpy-file-format"
   "asdf")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "iterative-methods")
   (:file "linear-algebra")
   (:file "wave-equation")
   (:file "image-processing")
   (:module "mnist-data"
    :components
    ((:static-file "test-images.npy")
     (:static-file "test-labels.npy")
     (:static-file "train-images.npy")
     (:static-file "train-labels.npy")))
   (:file "multigrid")
   (:file "mnist")))
