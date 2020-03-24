;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(document-function β*
  "Performs a reduction with the supplied binary function F and the initial
value Z on the array X.  If the optional argument AXIS is not supplied, the
reduction is carried out on all axes and the result is a scalar.  If it is
supplied, the reduction is only carried out on this particular axis."
  (compute (β* #'+ 0 (empty-array)))
  (compute (β* #'+ 0 #2a((1 2) (3 4))))
  (compute (β* #'+ 0 #2a((1 2) (3 4)) 0))
  (compute (β* #'+ 0 #2a((1 2) (3 4)) 1)))

(document-function vectorize
  "Turns the supplied function into a lazy, vector-valued Petalisp function.
The desired number of return values can be supplied as an optional second
argument."
  (compute (funcall (vectorize #'+) #(1 2 3 4) 5))
  (let ((fn (vectorize #'floor 2)))
    (multiple-value-bind (quot rem)
        (funcall fn #(1 2 3 4) #(4 3 2 1))
      (compute quot rem))))

(document-function collapse
  "Turns the supplied array into an array with the same rank and contents,
but where all ranges start from zero and have a step size of one."
  (collapse (reshape 42 (~ 1 3 99 ~ 1 8 99))))

(document-function flatten
  "Turns the supplied array into a rank one array, while preserving the
lexicographic ordering of the elements."
  (compute (flatten #2a((1 2) (3 4))))
  (compute (flatten #3a(((1 2) (3 4))
                        ((5 6) (7 8))))))

(document-function slice
  "For a supplied ARRAY with rank n, returns an array of rank n-1 that
contains all entries that have the supplied INDEX in the position specified
by AXIS."
  (compute (slice #(1 2 3 4) 2))
  (compute (slice #2A((1 2) (3 4)) 0))
  (compute (slice #2A((1 2) (3 4)) 1))
  (compute (slice #2A((1 2) (3 4)) 0 1))
  (compute (slice #2A((1 2) (3 4)) 1 1)))

(document-function slices
  "Selects those elements from ARRAY whose indices at the specified AXIS
are contained in the supplied RANGE."
  (compute (slices #(1 2 3 4) (range 0 2 2)))
  (compute (slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 2)))
  (compute (slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 0 2 2)))
  (compute (slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 0 2 2)
            1)))

(document-function stack
  "Stacks multiple array next to each other along the specified AXIS.  That
  means that along this axis, the leftmost array will have the lowest
  indices, and the rightmost array will have the highest indices."
  (compute (stack 0 #(1 2) #(3 4) #(5 6)))
  (compute (stack 0 #2A((1 2) (3 4)) #2A((5 6) (7 8))))
  (compute (stack 1 #2A((1 2) (3 4)) #2A((5 6) (7 8)))))
