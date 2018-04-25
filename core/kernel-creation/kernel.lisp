;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/kernel-creation/kernel
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all)
  (:export
   #:make-kernel
   #:kernel
   #:kernel?
   #:kernel-bounds
   #:kernel-references
   #:kernel-unknown-functions
   #:kernel-blueprint))

(in-package :petalisp/core/kernel-creation/kernel)

(defstruct (kernel
            (:copier nil)
            (:predicate kernel?))
  "A kernel is the fundamental unit of work in Petalisp. It describes how
some array elements are derived from others."
  (bounds            nil :type (simple-array array-index (*)))
  (references        nil :type (simple-array t (*)))
  (unknown-functions nil :type (simple-array function (*)))
  (blueprint         nil :type ulist))
