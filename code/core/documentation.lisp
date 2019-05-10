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
  "Returns a lazy array of integers of the shape indicated by the first
argument ARRAY-OR-SHAPE , where each array element at index (i_0 ... i_N)
has the value i_AXIS.  If AXIS is not supplied, it defaults to zero.

Examples:
 (compute (indices #2a((1 2) (3 4))))
  => #2a((0 0) (1 1))

 (compute (indices #2a((1 2) (3 4)) 1))
  => #2a((0 1) (0 1))

 (compute (indices (reshape #2a((1 2) (3 4)) (τ (i j) (i (1+ j)))) 1))
  => #2a((1 2) (1 2))

 (compute (indices \"abc\"))
  => #(0 1 2)
")

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
