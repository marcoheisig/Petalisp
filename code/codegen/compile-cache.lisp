;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

(defstruct compile-cache
  (table (make-hash-table :test #'eql . #+sbcl (:synchronized t)
                                        #-sbcl ())
   :type hash-table
   :read-only t))
