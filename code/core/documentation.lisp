;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(document-function broadcast-arrays
  "Returns as many lazy arrays as there are supplied arrays, but broadcast
such that all resulting arrays have the same shape.  If there is no
suitable broadcast shape for all supplied arrays, an error is signaled.

Examples:

 (broadcast-arrays #(1 2 3) 5)
  => #<array-immediate #(1 2 3)>
  => #<reference (unsigned-byte 4) (~ 0 2)>

 (broadcast-arrays #(2 3 4) #2a((1 2 3) (4 5 6)))
  => #<reference t (~ 0 1 ~ 0 2)>
  => #<array-immediate #2A((1 2 3) (4 5 6))>

")

(document-function broadcast-list-of-arrays
  "Returns a list of lazy arrays of the same length as the list of supplied
arrays, but where each element is broadcast such that all resulting arrays
have the same shape.  If there is no suitable broadcast shape for all
supplied arrays, an error is signaled.

Examples:

 (petalisp.core::broadcast-list-of-arrays (list #(1 2 3) 5))
  => (#<array-immediate #(1 2 3)> #<reference (unsigned-byte 4) (~ 0 2)>)

 (broadcast-list-of-arrays (list #(2 3 4) #2a((1 2 3) (4 5 6))))
  => (#<reference t (~ 0 1 ~ 0 2)> #<array-immediate #2A((1 2 3) (4 5 6))>)
")

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
  "Returns one or more lazy arrays whose contents are the values returned
by the supplied function, when applied element-wise to the contents of the
supplied arrays.  If the supplied arrays don't agree in shape, they are
first broadcast with the function BROADCAST-ARRAYS.

Examples:

 (compute (α #'+ 2 3))
  => 5

 (compute (α #'+ 2 #(1 2 3 4 5)))
  => #(3 4 5 6 7)

 (compute (α #'* #(2 3) #2a((1 2) (3 4))))
  => #2A((2 4) (9 12))

 (compute (α #'floor 7.5))
  => 1

 (compute (α #'floor 7.5 #(1 2 3 4 5)))
  => #(7 3 2 1 1)

 (multiple-value-bind (quot rem)
   (α #'floor 7.5 #(1 2 3 4 5))
   (compute quot rem))
  => #(7 3 2 1 1)
  => #(0.5 1.5 1.5 3.5 2.5)
")

(document-function β
  "Returns one or more lazy arrays whose contents are the multiple value
reduction with the supplied function, when applied pairwise to the elements
of the first axis of each of the supplied arrays.  If the supplied arrays
don't agree in shape, they are first broadcast with the function
BROADCAST-ARRAYS.

The supplied function F must accept 2k arguments and return k values, where
k is the number of supplied arrays.  All supplied arrays must have the same
shape S, which is the cartesian product of some ranges, i.e., S = r_1 x
... r_n, where each range r_k is a set of integers, e.g., {0, 1, ..., m}.
Then β returns k arrays of shape s = r_2 x ... x r_n, whose elements are a
combination of the elements along the first axis of each array according to
the following rules:

1. If the given arrays are empty, return k empty arrays.

2. If the first axis of each given array contains exactly one element, drop
   that axis and return arrays with the same content, but with shape s.

3. If the first axis of each given array contains more than one element,
   partition the indices of this axis into a lower half l and an upper half
   u.  Then split each given array into a part with shape l x s and a part
   with shape u x s.  Recursively process the lower and the upper halves of
   each array independently to obtain 2k new arrays of shape s.  Finally,
   combine these 2k arrays element-wise with f to obtain k new arrays with
   all values returned by f. Return these arrays.

Examples:
 (compute (β #'+ #(1 2 3 4)))
  => 10

 (compute (β #'+ #2a((1 2) (3 4))))
  => #(4 6)

 (let ((a #(5 2 7 1 9)))
   (multiple-value-bind (max index)
       (β (lambda (lv li rv ri)
            (if (> lv rv)
                (values lv li)
                (values rv ri)))
          a (indices a 0))
     (compute max index)))
  => 9
  => 4
")

(document-function reshape
  "Return a data structure of given SHAPE, either by selecting a subset of
the elements of DATA, or by broadcasting them.

Examples:

 (compute (reshape 4 (~ 0 4)))
  => #(4 4 4 4 4)

 (compute (reshape 0 (~ 0 1 ~ 0 1)))
  => #2A((0 0) (0 0))

 (compute (reshape #(1 2 3 4) (~ 1 2)))
  => #(2 3)
")

(document-function fuse
  "Combine ARRAYS into a single strided array.  It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion.

Examples:

 (compute (fuse (reshape 1 (~ 0 1))
                (reshape 0 (~ 2 3))))
  => #*1100

 (compute (fuse (reshape 1 (~ 0 2 6))
                (reshape 0 (~ 1 2 6))))
  => #*1010101
")

(document-function fuse*
  "Combine ARRAYS into a single strided array.  When some of the supplied
arguments overlap partially, the value of the rightmost object is used.

Examples:

 (compute (fuse* (reshape 1 (~ 0 3))
                 (reshape 0 (~ 2 3))))
  => #*1100
")
