;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defmacro document-variable (name &body body)
  (assert (boundp name))
  `(setf (documentation ',name 'variable)
         (progn ,@body)))

(defmacro document-function (name &body body)
  (assert (fboundp name))
  `(setf (documentation ',name 'function)
         (progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Docstrings

(document-function indices
  "Return an array of integers, where the value of each entry (i_0 ... i_N)
is i_AXIS.  If axis is not supplied, it defaults to zero.")

(document-function α
  ;; TODO
  "")

(document-function β
  ;; TODO
  "")

(document-function reshape
  "Return a data structure of given SHAPE, either by selecting a subset of
the elements of DATA, or by broadcasting them.

Examples:
 (reshape 0 (~ 10 ~ 10))        ; Create a 10x10 array of zeros
 (reshape #(1 2 3 4) (~ 1 2))   ; Select the two interior entries")

(document-function fuse
  "Combine ARRAYS into a single strided array.  It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion.")

(document-function fuse*
  "Combine ARRAYS into a single strided array.  When some of the supplied
arguments overlap partially, the value of the rightmost object is used.")
