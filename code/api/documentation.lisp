;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(document-type range
  "A range denotes a possibly empty set of integers.")

(document-type empty-range
  "An empty range is a range with zero elements.")

(document-type non-empty-range
  "A non-empty range denotes a set of integers, starting from a lower bound START,
by a fixed stride STEP, to an exclusive upper bound END.")

(document-function range
  "Returns a new, normalized range from the supplied parameters.

This function can be invoked with one, two or three integers.  In the case
of a single integer, it constructs the range from zero with step size one
up to but excluding that integer.  In the case of two integers, it creates
a range from the first integer with step size one up to but excluding the
second integer.  In the case of three arguments, it creates a range from
the first integer up to but excluding the second integer, using the third
integer as the step size.  If the first, inclusive integer and final,
exclusive integer are not congruent modulo the step size, the latter one is
moved towards the former until they are."
  (range 5)
  (range 5 9)
  (range 5 11)
  (range 5 13 2)
  (range 5 14 2)
  (range 7 -3 3))

(document-function rangep
  "Checks whether a supplied object is a range."
  (rangep 42)
  (rangep (range 1 3 2)))

(document-function size-one-range-p
  "Checks whether the supplied range has a size of one."
  (size-one-range-p (range 5))
  (size-one-range-p (range 5 7 2))
  (size-one-range-p (range 5 7 3)))

(document-function split-range
  "Splits the supplied range R into a lower and an upper half and returns
them as multiple values.  In case R has an odd number of element, the lower
half will have one more element than the upper half.

An error is signaled if the supplied range has only a single element."
  (split-range (range 1))
  (split-range (range 1 10))
  (split-range (range 1 9))
  (split-range (range 2 9 2)))

(document-function map-range
  "Takes a function and a range and applies the function to all integers of
that range, in ascending order."
  (let ((l '()))
    (map-range (lambda (i) (push i l)) (range 1 9 2))
    (nreverse l)))

(document-function range-equal
  "Check whether two supplied ranges describe the same set of integers."
  (range-equal (range 1) (range 2))
  (range-equal (range 2) (range 2))
  (range-equal (range 0 8 2) (range 0 9 2))
  (range-equal (range 0 8 3) (range 0 9 3)))

(document-function range-contains
  "Check whether the supplied range contains a particular integer."
  (range-contains (range 1 10) 5)
  (range-contains (range 1 10) -5)
  (range-contains (range 1 10 3) 4))

(document-function range-intersection
  "Returns the range containing exactly those elements that occur in both
supplied ranges.."
  (range-intersection (range 1 10) (range 2 20))
  (range-intersection (range 3 14 2) (range 1 14 3)))

(document-function range-intersectionp
  "Check whether two supplied ranges have at least one common element."
  (range-intersectionp (range 1 10) (range 2 20))
  (range-intersectionp (range 0 7 2) (range 1 8 2)))

(document-function range-difference-list
  "Compute the difference of two ranges R1 and R2.  Returns a list of
disjoint subranges of R1 that describe exactly those integers appearing in
R1 but not in R2.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shapes

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations

(document-function transform
  "Creates a transformation that maps some inputs to the specified outputs.
The syntax is (transform INPUT-1 ... INPUT-N to OUTPUT-1 OUTPUT-N).

Each input can either be a variable, an integer, or a list whose first
element is a variable and whose second element is a form that evaluates
to an integer.

Each output is a form that may reference up to one of the input variables.
The result of an output form in a context where the referenced input
variable is bound to an integer must be an integer.  The mapping form the
input to the output must be linear."
  (transform i to (+ i 1))
  (transform i to (+ (+ i 1) 5))
  (transform 1 2 3 to)
  (transform to 1 2 3)
  (transform i j to j i))

(document-type transformation
  "A transformation with input rank N and output rank M is a mapping
from lists of length N to lists of rank M.")

(document-type identity-transformation
  "An identity transformation of rank N maps every list of length N to
itself.  An identity transformation is its own inverse.")

(document-function transformation-invertiblep
  "Check whether a supplied transformation is invertible."
  (transformation-invertiblep (transform i j to j i))
  (transformation-invertiblep (transform i j to i)))

(document-function transformation-equal
  "Check whether two supplied transformations describe the same mapping."
  (transformation-equal
   (transform i to (* 2 (1+ i)))
   (transform i to (+ 2 (* 2 i))))
  (transformation-equal
   (transform i j to i j)
   (transform i j to j i)))

(document-function transformation-similar
  "Check whether two supplied transformations are similar.  Two
transformations are similar if they have the same permutation, the same
inputs constraints, the same scalings, and offsets whose entries differ in
at most DELTA."
  (transformation-similar
   (transform a to a)
   (transform a to (1+ a))
   0)
  (transformation-similar
   (transform a to a)
   (transform a to (1+ a))
   1)
  (transformation-similar
   (transform i j to (+ j 2) i)
   (transform i j to (- j 1) i)
   2)
  (transformation-similar
   (transform i j to (+ j 2) i)
   (transform i j to (- j 1) i)
   3))

(document-function compose-transformations
  "Returns a single transformation that is equivalent to consecutive
invocations of the supplied transformations in right-to-left order."
  (compose-transformations
   (transform i to (* 2 (1+ i)))
   (transform i to (1- (/ i 2))))
  (compose-transformations
   (transform i j to (+ i 5) (+ j 7))
   (transform i j to (* j 2) (* i 3))))

(document-function invert-transformation
  "Returns the inverse of the supplied transformation.

An error is signaled if the supplied transformation is not invertible."
  (invert-transformation
   (transform i to (+ 2 i)))
  (invert-transformation
   (transform a b to (+ (* 2 b) 5) (+ (* 3 a) 7)))
  (invert-transformation
   (transform a 0 to a))
  (invert-transformation
   (transform a b to a)))

(document-function map-transformation-inputs
  "For each input of TRANSFORMATION, invoke FUNCTION with the input index
and the corresponding input constraint, or null, if there is no input
constraint for this input.

If FROM-END is false, the input indices are traversed in ascending order.
Otherwise, they are traversed in descending order.")

(document-function map-transformation-outputs
  "For each output of TRANSFORMATION, invoke FUNCTION with the output
index, input index, the scaling and the offset of that output.

An input index of NIL and a scaling of zero is used, if (and only if) the
output is constant.

If FROM-END is false, the output indices are traversed in ascending order.
Otherwise, they are traversed in descending order.")

(document-function enlarge-transformation
  "Given a transformation mapping from (i1 ... iN) to (j1 ... jM),
return a transformation mapping from (i0 i1 ... iN iN+1) to
((+(* i0 SCALE) OFFSET) j1 ... jM).")

(document-function identity-transformation
  "Returns an identity transformation of the specified rank.")

(document-function make-transformation
  "Returns a transformation according to the supplied keyword arguments.
Valid keyword arguments are:

:INPUT-RANK

The length or rank of any valid transformation input.  A non-negative
integer.

:OUTPUT-RANK

The length or rank of any transformation output.  A non-negative integer.

:INPUT-MASK

A sequence with as many elements as the input rank of the transformation.
Each element must either be an integer, in which case only this integer may
occur in the corresponding input position, or NIL, in which case any
integer may occur in the corresponding input position.

:OUTPUT-MASK

A sequence with as many elements as the output rank of the transformation.
Every element must either be an integer, in which case this integer denotes
the input entry that is to be scaled, shifted and sent to the current
position's output, or NIL, in which case only the corresponding offset
value is sent to the current output.  This way, the output mask can encode
both permutations of the input, as well as insertion and removal of axes.
If this keyword argument is not supplied, it defaults to a sequence with up
to input rank ascending integers, starting from zero, and followed by
entries of NIL in case the output rank is larger than the input rank.

:SCALINGS

A sequence with as many elements as the output rank of the transformation.
Each element must be a rational number.  Every transformation output is
scaled with its corresponding entry in this sequence.  If this keyword
argument is not supplied, it defaults to a sequence of ones.

:OFFSETS

A sequence with as many elements as the output rank of the transformation.
Each element must be a rational number that is added to the corresponding
output value after permutation and scaling has taken place. If this keyword
argument is not supplied, it defaults to a sequence of zeros."
  (make-transformation :input-rank 2)
  (make-transformation :input-rank 2 :output-rank 1)
  (make-transformation :input-mask '(2 nil 3))
  (make-transformation :output-mask #(1 0 nil))
  (make-transformation :offsets #(1 2 3) :scalings #(4 5 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy Arrays

(document-type lazy-array
  "A lazy array encapsulates some information that can be used to compute
actual Common Lisp arrays.")

(document-function lazy-reshape
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
  (compute (lazy-reshape 4 (~ 0 4)))
  (compute (lazy-reshape #(1 2 3 4) (~ 1 2)))
  (compute (lazy-reshape (lazy-shape-indices (~ 9)) (~ 3 ~ 3)))
  (compute (lazy-reshape #2A((1 2) (3 4)) (transform i j to j i)))
  (compute (lazy-reshape #(1 2 3 4) (~ 1 3) (~ 0 2 ~ 0 2))))

(document-function lazy-broadcast-arrays
  "Returns as many lazy arrays as there are supplied arrays, but broadcast
such that all resulting arrays have the same shape.  If there is no
suitable broadcast shape for all supplied arrays, an error is signaled."
  (lazy-broadcast-arrays #(1 2 3) 5)
  (lazy-broadcast-arrays #(2 3 4) #2a((1 2 3) (4 5 6))))

(document-function lazy-broadcast-list-of-arrays
  "Returns a list of lazy arrays of the same length as the list of supplied
arrays, but where each element is broadcast such that all resulting arrays
have the same shape.  If there is no suitable broadcast shape for all
supplied arrays, an error is signaled."
  (lazy-broadcast-list-of-arrays (list #(1 2 3) 5))
  (lazy-broadcast-list-of-arrays (list #(2 3 4) #2a((1 2 3) (4 5 6)))))

(document-function lazy
  "Returns one or more lazy arrays, whose contents are the values returned
by the supplied function when applied element-wise to the contents of the
remaining argument arrays.  If the arguments don't agree in shape, they are
first broadcast with the function BROADCAST-ARRAYS."
  (lazy #'+ #(1 2) #(3 4))
  (compute (lazy #'+ 2 3))
  (compute (lazy #'+ 2 #(1 2 3 4 5)))
  (compute (lazy #'* #(2 3) #2a((1 2) (3 4))))
  (compute (lazy #'floor 7.5))
  (compute (lazy #'floor 7.5 #(1 2 3 4 5))))

(document-function lazy-collapse
  "Turns the supplied array into an array with the same rank and contents,
but where all ranges start from zero and have a step size of one."
  (lazy-collapse (lazy-reshape 42 (~ 1 100 3 ~ 1 100 8))))

(document-function lazy-fuse
  "Combine ARRAYS into a single strided array.  It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion."
  (compute (lazy-fuse (lazy-reshape 1 (~ 0 2))
                      (lazy-reshape 0 (~ 2 4))))
  (compute (lazy-fuse (lazy-reshape 1 (~ 0 7 2))
                      (lazy-reshape 0 (~ 1 7 2)))))

(document-function lazy-overwrite
  "Combines ARRAYS into a single strided array.  When some of the supplied
arguments overlap partially, the value of the rightmost object is used."
  (compute (lazy-overwrite (lazy-reshape 1 (~ 0 4))
                           (lazy-reshape 0 (~ 2 4)))))

(document-function lazy-array-indices
  "Returns a lazy array of integers of the shape of ARRAY, where each array
element at index (i_0 ... i_N) has the value i_AXIS.  If AXIS is not
supplied, it defaults to zero."
  (compute (lazy-array-indices #2a((1 2) (3 4))))
  (compute (lazy-array-indices #2a((1 2) (3 4)) 1))
  (compute (lazy-array-indices "abc")))

(document-function lazy-shape-indices
  "Returns a lazy array of integers of the shape of SHAPE, where each
array element at index (i_0 ... i_N) has the value i_AXIS.  If AXIS is not
supplied, it defaults to zero."
  (compute (lazy-shape-indices (~ 9)))
  (compute (lazy-shape-indices (~ 0 4 2 ~ 1 5 2) 0))
  (compute (lazy-shape-indices (~ 0 4 2 ~ 1 5 2) 1)))

(document-function lazy-array-interior
  "For a given ARRAY and WIDTH, return a new, lazy array with those
  elements that are at least WIDTH entries away from any boundary.  If not
  supplied, WIDTH defaults to one."
  (compute (lazy-array-interior #2a((1 2 3 4) (5 6 7 8) (1 2 3 4) (5 6 7 8))))
  (compute (lazy-array-interior #2a((1 2 3 4) (5 6 7 8) (1 2 3 4) (5 6 7 8)) 2)))

(document-function lazy-drop-axes
  "Removes zero or more axes whose corresponding range has only a single
element from a supplied array."
  (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 3)) 1)
  (compute (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 3)) 1))
  (compute (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 3)) 0 1))
  (compute (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 5)) 0)))

(document-function lazy-flatten
  "Turns the supplied array into a rank one array, while preserving the
lexicographic ordering of the elements."
  (compute (lazy-flatten #2a((1 2) (3 4))))
  (compute (lazy-flatten #3a(((1 2) (3 4)) ((5 6) (7 8))))))

(document-function lazy-slice
  "For a supplied ARRAY with rank n, returns an array of rank n-1 that
contains all entries that have the supplied INDEX in the position specified
by AXIS."
  (compute (lazy-slice #(1 2 3 4) 2))
  (compute (lazy-slice #2A((1 2) (3 4)) 0))
  (compute (lazy-slice #2A((1 2) (3 4)) 1))
  (compute (lazy-slice #2A((1 2) (3 4)) 0 1))
  (compute (lazy-slice #2A((1 2) (3 4)) 1 1)))

(document-function lazy-slices
  "Selects those elements from ARRAY whose indices at the specified AXIS
are contained in the supplied RANGE."
  (compute (lazy-slices #(1 2 3 4) (range 0 2 2)))
  (compute (lazy-slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 2)))
  (compute (lazy-slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 0 3 2)))
  (compute (lazy-slices
            #2A((1 0 0)
                (0 1 0)
                (0 0 1))
            (range 0 3 2)
            1)))

(document-function lazy-stack
  "Stacks multiple array next to each other along the specified AXIS such
that along this axis, the leftmost array will have the lowest indices, and
the rightmost array will have the highest indices."
  (compute (lazy-stack 0 #(1) #(2) #(3)))
  (compute (lazy-stack 0 #(1 2) #(3 4) #(5 6)))
  (compute (lazy-stack 0 #2A((1 2) (3 4)) #2A((5 6) (7 8))))
  (compute (lazy-stack 1 #2A((1 2) (3 4)) #2A((5 6) (7 8)))))

(document-function lazy-reduce
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
  (compute (lazy-reduce #'+ #(1 2 3 4)))
  (compute (lazy-reduce #'+ #2a((1 2) (3 4))))
  (let ((a #(5 2 7 1 9)))
    (multiple-value-bind (max index)
        (lazy-reduce
         (lambda (lv li rv ri)
           (if (> lv rv)
               (values lv li)
               (values rv ri)))
         a (lazy-array-indices a 0))
      (compute max index))))

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
  (compute (lazy #'+ 2 #(3 4)))
  (compute (lazy-reshape #2a((1 2) (3 4)) (transform i j to j i)))
  (compute 2 #0A3 #(4)))

(document-function compute-list-of-arrays
  "Returns a list of computed results - one for each element in the list
of supplied arrays.

The computed result of an array or lazy array with rank zero is the one
element contained in this array.  The computed result of any other array or
lazy array is an array with the same rank and dimensions.  The computed
result of any other object is that object itself."
  (compute-list-of-arrays (list (lazy #'+ 2 #(3 4))))
  (compute-list-of-arrays (list (lazy-reshape #2a((1 2) (3 4)) (transform i j to j i))))
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

(document-function wait
  "Blocks until the work designated by some SCHEDULE operations has been
completed.  Each argument must be one of the opaque objects returned by
calling SCHEDULE.")

(document-function prepare
  "Returns, as multiple values, the array immediates corresponding to the
supplied arrays."
  (prepare 1 #(2 3))
  (prepare (lazy #'+ 2 3)))

(document-function prepare-list-of-arrays
  "Returns a list of array immediates, one for each array in the supplied
list of arrays."
  (prepare-list-of-arrays (list 1 #(2 3)))
  (prepare-list-of-arrays (list (lazy #'+ 2 3))))
