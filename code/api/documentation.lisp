;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(document-variables (~ ~l ~s ~r)
  "The symbols ~, ~l, ~s and ~r are self-evaluating.  Their only purpose is
to separate range designators in the functions named ~, ~l, ~s and ~r.")

(document-functions (~ ~l ~s ~r)
  "The functions ~, ~l, ~s and ~r can be used to construct new shapes from
some given range designators, from lists of ranges, from shapes, or from a
single range, respectively.  Each of these patterns can be used repeatedly
in a single call."
  (~)
  (~ 8)
  (~ 1 10)
  (~ 0 10 2 ~ 0 10 2)
  (~ 5 ~s (~ 1 4 2 ~ 4 10 5) ~ 5)
  (~r (range 1 10) ~l (list (range 2 9)) ~ 42)
  (apply #'~ 1 10 (loop repeat 3 append '(~ 2 6))))

(document-type shape
  "A shape is the cartesian product of zero or more ranges.  Shapes can be
constructed by calling ~ or MAKE-SHAPE.  The elements of a shape are lists
of integers.  The rank of a shape is the length of these lists.  For
example, the shape (~ 0 1 ~ 1 3 ~ 3 8 4) has rank three and consists of the
integer tuples (0 1 3), (0 1 7), (0 2 3), (0 2 7).")

(document-function shapep
  "Checks whether a supplied object is a shape."
  (shapep 42)
  (shapep (~ 1 ~ 2 ~ 3 4)))

(document-function shape-rank
  "Returns the rank of the supplied shape, i.e., the number of ranges it
contains."
  (shape-rank (~))
  (shape-rank (~ 1 ~ 2 ~ 3))
  (shape-rank (~ 0 9 ~ 0 9)))

(document-function shape-ranges
  "Returns a list of all ranges contained in the supplied shape."
  (shape-ranges (~))
  (shape-ranges (~ 1 ~ 2 ~ 3))
  (shape-ranges (~ 0 9 ~ 0 9)))

(document-function shape-dimensions
  "Return the array dimensions corresponding to a shape.  Signal an error
if any of the ranges of the shape have a nonzero start or a step size other
than one."
  (shape-dimensions (~))
  (shape-dimensions (~ 0 9))
  (shape-dimensions (~ 1 9))
  (shape-dimensions (~ 0 2 9))
  (shape-dimensions (~ 0 4 ~ 0 5 ~ 0 6)))

(document-function shape-size
  "Returns that number of integer tuples denoted by the supplied shape."
  (shape-size (~))
  (shape-size (~ 2 9))
  (shape-size (~ 1 9 ~ 1 8)))

(document-function shape-equal
  "Checks whether two supplied shapes denote the same set of integer tuples."
  (shape-equal (~) (~))
  (shape-equal (~ 42) (~ 42))
  (shape-equal (~ 1 42) (~ 1 42))
  (shape-equal (~ 1 42) (~ 2 42)))

(document-function shape-difference-list
  "Computes the difference of two shapes S1 and S2.  Returns a list of
disjoint subshapes of S1 that describe exactly those integer tuples
appearing in S1 but not in S2."
  (shape-difference-list (~ 1 11) (~ 2 10))
  (shape-difference-list (~ 1 11) (~ 4 8))
  (shape-difference-list (~ 1 11) (~ 2 9 2))
  (shape-difference-list (~ 1 11) (~ 2 21))
  (shape-difference-list (~ 1 21 2) (~ 1 21 3)))

(document-function shape-intersection
  "Returns the shape containing exactly those integer tuples that occur in
both supplied shapes.  Returns NIL if there are no such elements."
  (shape-intersection (~ 1 11 ~ 3 14 2) (~ 1 6 ~ 1 14 3))
  (shape-intersection (~ 1 6) (~ 6 11)))

(document-function shape-intersectionp
  "Check whether two supplied shapes have at least one common element."
  (shape-intersectionp (~ 1 6) (~ 6 10))
  (shape-intersectionp (~ 1 5) (~ 6 10)))

(document-function map-shape
  "Takes a function and a shape and applies the function to all integer
tuples of that range, in ascending order."
  (let ((l '()))
    (map-shape (lambda (i) (push i l)) (~ 1 3 ~ 3 5))
    (nreverse l)))

(document-function shape-contains
  "Check whether the supplied shape contains a particular integer tuple."
  (shape-contains (~ 1 9) (list 4))
  (shape-contains (~ 1 2 9) (list 4)))

(document-function shrink-shape
  "This function expects a single shape with one or more ranges R1 to Rn.
It returns a shape with the ranges R2 to R1, and, as a second value, the
range R1 that has been peeled off."
  (shrink-shape (~ 1 10))
  (shrink-shape (~ 1 10 ~ 0 2)))

(document-function enlarge-shape
  "For a given shape S and range R, this function returns a shape whose
  first range is R, and whose remaining ranges are those of S."
  (enlarge-shape (~) (range 1 10))
  (enlarge-shape (~ 1 3) (range 1 4)))

(document-function subdivide-arrays
  "Invoke SUBDIVIDE-SHAPES on the shapes of the supplied ARRAYS."
  (subdivide-arrays (list))
  (subdivide-arrays (list #()))
  (subdivide-arrays (list #() #()))
  (subdivide-arrays (list #(1 2 3 4) #(1 2))))

(document-function subdivide-shapes
  "Returns a list of (shape . bitmask) conses.  Each shape is a proper
subshape of one or more of the supplied shapes and their fusion covers all
supplied shapes.  The bitmask indicates which of the supplied shapes are
supersets of the corresponding resulting shape."
  (subdivide-shapes (list (~ 1 3 ~ 1 3) (~ 1 2 ~ 1 2)))
  (subdivide-shapes (list (~ 1 10) (~ 2 20))))

(document-function subshapep
  "Checks for two shapes whether the former is fully contained in the
latter."
  (subshapep (~) (~))
  (subshapep (~ 0 9) (~ 0 9))
  (subshapep (~ 0 3) (~ 1 9))
  (subshapep (~ 0 3 ~ 0 3) (~ 0 9 ~ 0 9)))

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
  (compute (reshape (shape-indices (~ 9)) (~ 3 ~ 3)))
  (compute (reshape #2A((1 2) (3 4)) (τ (i j) (j i))))
  (compute (reshape #(1 2 3 4) (~ 1 3) (~ 0 2 ~ 0 2))))

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
  (collapse (reshape 42 (~ 1 100 3 ~ 1 100 8))))

(document-function fuse
  "Combine ARRAYS into a single strided array.  It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion."
  (compute (fuse (reshape 1 (~ 0 2))
                 (reshape 0 (~ 2 4))))
  (compute (fuse (reshape 1 (~ 0 7 2))
                 (reshape 0 (~ 1 7 2)))))

(document-function fuse*
  "Combines ARRAYS into a single strided array.  When some of the supplied
arguments overlap partially, the value of the rightmost object is used."
  (compute (fuse* (reshape 1 (~ 0 4))
                  (reshape 0 (~ 2 4)))))

(document-function array-indices
  "Returns a lazy array of integers of the shape of ARRAY, where each array
element at index (i_0 ... i_N) has the value i_AXIS.  If AXIS is not
supplied, it defaults to zero."
  (compute (array-indices #2a((1 2) (3 4))))
  (compute (array-indices #2a((1 2) (3 4)) 1))
  (compute (array-indices "abc")))

(document-function shape-indices
   "Returns a lazy array of integers of the shape of SHAPE, where each
array element at index (i_0 ... i_N) has the value i_AXIS.  If AXIS is not
supplied, it defaults to zero."
   (compute (shape-indices (~ 9)))
   (compute (shape-indices (~ 0 4 2 ~ 1 5 2) 0))
   (compute (shape-indices (~ 0 4 2 ~ 1 5 2) 1)))

(document-function array-interior
  "For a given ARRAY and WIDTH, return a new, lazy array with those
  elements that are at least WIDTH entries away from any boundary.  If not
  supplied, WIDTH defaults to one."
  (compute (array-interior #2a((1 2 3 4) (5 6 7 8) (1 2 3 4) (5 6 7 8))))
  (compute (array-interior #2a((1 2 3 4) (5 6 7 8) (1 2 3 4) (5 6 7 8)) 2)))

(document-function drop-axes
  "Removes zero or more axes whose corresponding range has only a single
element from a supplied array."
  (drop-axes (reshape 1 (~ 1 2 ~ 2 3)) 1)
  (compute (drop-axes (reshape 1 (~ 1 2 ~ 2 3)) 1))
  (compute (drop-axes (reshape 1 (~ 1 2 ~ 2 3)) 0 1))
  (compute (drop-axes (reshape 1 (~ 1 2 ~ 2 5)) 0)))

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
            (range 0 3 2)))
  (compute (slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 0 3 2)
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
          a (array-indices a 0))
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
argument.

The computed result of an array or lazy array with rank zero is the one
element contained in this array.  The computed result of any other array or
lazy array is an array with the same rank and dimensions.  The computed
result of any other object is that object itself."
  (compute (α #'+ 2 #(3 4)))
  (compute (reshape #2a((1 2) (3 4)) (τ (i j) (j i))))
  (compute 2 #0A3 #(4)))

(document-function compute-list-of-arrays
  "Returns a list of computed results - one for each element in the list
of supplied arrays.

The computed result of an array or lazy array with rank zero is the one
element contained in this array.  The computed result of any other array or
lazy array is an array with the same rank and dimensions.  The computed
result of any other object is that object itself."
  (compute-list-of-arrays (list (α #'+ 2 #(3 4))))
  (compute-list-of-arrays (list (reshape #2a((1 2) (3 4)) (τ (i j) (j i)))))
  (compute-list-of-arrays (list 2 #0A3 #(4))))

(document-function schedule
  "Hints that it would be worthwhile to compute the supplied arrays
asynchronously.  Returns an opaque object that can be supplied to WAIT to
wait until the scheduled operation has been performed.

This function allows speeding up certain programs like

 (progn (run-expensive-task)
        (compute array-1 array-2))

by rewriting them to something like

 (progn (schedule array-1 array-2)
        (run-expensive-task)
        (compute array-1 array-2)).")

(document-function schedule-list-of-arrays
  "Hints that it would be worthwhile to compute all arrays in the supplied
list of arrays asynchronously.  Returns an opaque object that can be
supplied to WAIT to wait until the scheduled operation has been performed.

This function allows speeding up certain programs like

 (progn (run-expensive-task)
        (compute-list-of-arrays l))

by rewriting them to something like

 (progn (schedule-list-of-arrays l)
        (run-expensive-task)
        (compute-list-of-arrays l)).")

(document-function wait
  "Blocks until the work designated by some SCHEDULE operations has been
completed.  Each argument must be one of the opaque objects returned by
calling SCHEDULE.")

(document-function prepare
  "Returns, as multiple values, the array immediates corresponding to the
supplied arrays."
  (prepare 1 #(2 3))
  (prepare (α #'+ 2 3)))

(document-function prepare-list-of-arrays
  "Returns a list of array immediates, one for each array in the supplied
list of arrays."
  (prepare-list-of-arrays (list 1 #(2 3)))
  (prepare-list-of-arrays (list (α #'+ 2 3))))
