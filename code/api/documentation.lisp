;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(document-function reshape
  "Returns a lazy array with the contents of ARRAY, but after applying the
supplied MODIFIERS in left-to-right order.  A modifier must either be a
shape, or a transformation.

A shape can denote one of three different modifications, depending on
whether it is larger than the array, smaller than the array, or has the
same number of elements as the array.  If the shape is larger than the
array, it denotes a broadcasting operation.  If the shape is smaller than
the array, it denotes a selection of a sub-array.  If the shape has the
same number of elements, it denotes a lexicographic reordering operation.

In case the modifier is a transformation, the new array is obtained by
taking each index and corresponding value of the original array and
applying the transformation to the index while retaining the value."
  (compute (reshape 4 (~ 0 4)))
  (compute (reshape #(1 2 3 4) (~ 1 2)))
  (compute (reshape (indices (~ 1 9)) (~ 0 2 ~ 0 2)))
  (compute (reshape #2A((1 2) (3 4)) (τ (i j) (j i))))
  (compute (reshape #(1 2 3 4) (~ 1 2) (~ 0 1 ~ 0 1))))

(document-function broadcast-arrays
  "Returns as many lazy arrays as there are supplied arrays, but broadcast
such that all resulting arrays have the same shape.  If there is no
suitable broadcast shape for all supplied arrays, an error is signaled."
  (broadcast-arrays #(1 2 3) 5)
  (broadcast-arrays #(2 3 4) #2a((1 2 3) (4 5 6))))

(document-function broadcast-list-of-arrays
  "Returns a list of lazy arrays of the same length as the list of supplied
arrays, but where each element is broadcast such that all resulting arrays
have the same shape.  If there is no suitable broadcast shape for all
supplied arrays, an error is signaled."
  (broadcast-list-of-arrays (list #(1 2 3) 5))
  (broadcast-list-of-arrays (list #(2 3 4) #2a((1 2 3) (4 5 6)))))

(document-function α
  "Returns one or more lazy arrays, whose contents are the values returned
by the supplied function when applied element-wise to the contents of the
remaining argument arrays.  If the arguments don't agree in shape, they are
first broadcast with the function BROADCAST-ARRAYS."
  (α #'+ #(1 2) #(3 4))
  (compute (α #'+ 2 3))
  (compute (α #'+ 2 #(1 2 3 4 5)))
  (compute (α #'* #(2 3) #2a((1 2) (3 4))))
  (compute (α #'floor 7.5))
  (compute (α #'floor 7.5 #(1 2 3 4 5))))

(document-function collapse
  "Turns the supplied array into an array with the same rank and contents,
but where all ranges start from zero and have a step size of one."
  (collapse (reshape 42 (~ 1 3 99 ~ 1 8 99))))

(document-function fuse
  "Combine ARRAYS into a single strided array.  It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion."
  (compute (fuse (reshape 1 (~ 0 1))
                 (reshape 0 (~ 2 3))))
  (compute (fuse (reshape 1 (~ 0 2 6))
                 (reshape 0 (~ 1 2 6)))))

(document-function fuse*
  "Combines ARRAYS into a single strided array.  When some of the supplied
arguments overlap partially, the value of the rightmost object is used."
  (compute (fuse* (reshape 1 (~ 0 3))
                 (reshape 0 (~ 2 3)))))

(document-function drop-axes
  "Removes zero or more axes whose corresponding range has only a single
element from a supplied array."
  (drop-axes (reshape 1 (~ 1 ~ 2)) 1)
  (compute (drop-axes (reshape 1 (~ 1 ~ 2)) 1))
  (compute (drop-axes (reshape 1 (~ 1 ~ 2)) 0 1))
  (compute (drop-axes (reshape 1 (~ 1 ~ 2 4)) 0)))

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
   all values returned by f. Return these arrays."
  (compute (β #'+ #(1 2 3 4)))
  (compute (β #'+ #2a((1 2) (3 4))))
  (let ((a #(5 2 7 1 9)))
   (multiple-value-bind (max index)
       (β (lambda (lv li rv ri)
            (if (> lv rv)
                (values lv li)
                (values rv ri)))
          a (indices a 0))
     (compute max index))))

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

(document-function differentiator
  "Returns a function that, for each node in a network whose roots are the
supplied OUTPUTS will return the gradient at that node.

GRADIENTS must be a sequence of the same length as OUTPUTS, and whose
elements are either arrays with or symbols that will be used as the name of
such a parameter.")

(document-type network
  "A network is an encapsulated data-flow graph that can be invoked with a
set of inputs and weights to yield several outputs.

Networks can also be differentiated, using the function NETWORK-GRADIENTS.")

(document-function make-network
  "Creates a network with the supplied inputs and outputs.

An error is signaled of any of the inputs is not of type NETWORK-INPUT, or
if additional network inputs are reachable from the network outputs.")

(document-function compute
  "Returns, as multiple values, the computed result of each supplied
argument.  The computed result of a lazy array is a standard Common Lisp
array with the same rank and dimensions.  The computed result of any other
object is that object itself."
  (compute (α #'+ 2 #(3 4)))
  (compute (reshape #2a((1 2) (3 4)) (τ (i j) (j i))))
  (compute 2 #0A3 #(4)))

(document-function schedule
  "Hints that it would be worthwhile to compute the supplied arguments
asynchronously.  Semantically, this function does nothing.  But on certain
backends, a program like

 (progn (run-expensive-task)
        (compute array-1 array-2))

can be sped up by rewriting it as

 (progn (schedule array-1 array-2)
        (run-expensive-task)
        (compute array-1 array-2)).")
