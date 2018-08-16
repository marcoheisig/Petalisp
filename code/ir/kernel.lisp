;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defstruct (kernel
            (:copier nil)
            (:predicate kernelp))
  "A kernel is the fundamental unit of work in Petalisp. It describes how
some array elements are derived from others."
  (bounds            nil :type (simple-array array-index (*)))
  (references        nil :type (simple-array t (*)))
  (unknown-functions nil :type (simple-array function (*)))
  (blueprint         nil :type ucons:ulist))
