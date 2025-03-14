(in-package #:petalisp.codegen)

(defstruct compile-cache
  (table (make-hash-table :test #'eql . #+sbcl (:synchronized t)
                                        #-sbcl ())
   :type hash-table
   :read-only t))
