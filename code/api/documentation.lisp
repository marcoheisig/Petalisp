;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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

This function can be invoked with one, two or three integers.  If it is
called with a single argument, the result is a range starting from zero,
with step size one, up to but excluding the supplied argument.  In other
words, a single argument is treated just like one of the dimensions of a
regular array.  If the range constructor is called with two arguments, then
the result is still a range with a step size of one, but with the first
argument as the inclusive lower bound, and with the second argument as the
exclusive upper bound.  The three argument version behaves just like the
two argument version, except that the additional third argument denotes the
step size.  The sign of the step size gives the direction of the range: If
the sign is positive, then the exclusive upper bound must be larger than
the inclusive lower bound or the resulting range is empty.  If the sign is
negative, the first argument is used as an inclusive upper bound, and the
second argument is used as an exclusive lower bound.

It is worth mentioning that the range constructor has the exact same name
and semantics as the range constructor in the Python programming language."
  (range 5)
  (range 5 9)
  (range 5 13 2)
  (range 5 14 2)
  (range 1 7 -2)
  (range 7 1 -2))

(document-function empty-range
  "Returns a range with size zero.")

(document-function rangep
  "Returns whether a supplied object is a range."
  (rangep 42)
  (rangep (range 1 3 2)))

(document-function size-one-range-p
  "Returns whether the supplied range has a size of one."
  (size-one-range-p (range 5))
  (size-one-range-p (range 5 7 2))
  (size-one-range-p (range 5 7 3)))

(document-function range-size
  "Returns the number of elements in the supplied range.")

(document-function range-start
  "Returns the lowest integer contained in the supplied range.  An error is
signaled in case the range has zero elements.")

(document-function range-step
  "Returns the difference between any two successive integers in the supplied
range.  An error is signaled in case the range has zero elements.")

(document-function range-last
  "Returns the highest integer contained in the supplied range.  An error is
signaled in case the range has zero elements.")

(document-function range-end
  "Returns an integer that is larger than any integer in the supplied
range.  An error is signaled in case the range has zero elements.")

(document-function split-range
  "Splits the supplied range R into a lower and an upper half and returns
those two halves as multiple values.  In case R has an odd number of
elements, the lower half will have one more element than the upper half.

The optional POSITION argument is a real number that can be used to
prescribe the point at which to split the range."
  (split-range (range 0))
  (split-range (range 1))
  (split-range (range 1 10))
  (split-range (range 1 10) 3)
  (split-range (range 1 10) 1)
  (split-range (range 1 10) 10)
  (split-range (range 1 9))
  (split-range (range 2 9 2))
  (split-range (range 2 9 2) 3))

(document-function map-range
  "Takes a function and a range and applies the function to all integers of
that range, in ascending order.  Returns the range being mapped over."
  (let ((l '()))
    (map-range (lambda (i) (push i l)) (range 1 9 2))
    (nreverse l)))

(document-function range=
  "Returns whether two supplied ranges describe the same set of integers."
  (range= (range 1) (range 2))
  (range= (range 2) (range 2))
  (range= (range 0 8 2) (range 0 9 2))
  (range= (range 0 8 3) (range 0 9 3)))

(document-function range-contains
  "Returns whether the supplied range contains a particular integer."
  (range-contains (range 1 10) 5)
  (range-contains (range 1 10) -5)
  (range-contains (range 1 10 3) 4))

(document-function range-intersection
  "Returns the range containing exactly those elements that occur in both
supplied ranges.."
  (range-intersection (range 1 10) (range 2 20))
  (range-intersection (range 3 14 2) (range 1 14 3)))

(document-function range-intersectionp
  "Returns whether two supplied ranges have at least one common element."
  (range-intersectionp (range 1 10) (range 2 20))
  (range-intersectionp (range 0 7 2) (range 1 8 2)))

(document-function range-difference-list
  "Compute the difference of the two supplied ranges RANGE1 and RANGE2.
Returns a list of disjoint subranges of RANGE1 that describe exactly those
integers appearing in RANGE1 but not in RANGE2.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shapes

(document-variables (~ ~*)
  "The symbols ~, ~* are self-evaluating.  Their only purpose is to separate
range designators in the functions named ~, ~*.")

(document-functions (~ ~*)
  "Returns a shape whose ranges are derived by looking at each occurrence of
one of the self-evaluating delimiter symbols ~ and ~*, and the arguments
following such a delimiter up to the next one.  Each such group contributes
one or more ranges to the resulting shape.  The behavior of each delimiter
is as follows:

- The ~ delimiter must be followed by one, two, or three integers that are
  then supplied to the RANGE function to construct the single resulting
  range.

- The ~* delimiter must be followed by any number of ranges that are
  incorporated into the resulting shape as they are."
  (~)
  (~ 8)
  (~ 1 10)
  (~ 0 10 2 ~ 0 10 2)
  (~* (range 1 10) (range 2 9) ~ 42)
  (apply #'~ 1 10 (loop repeat 3 append '(~ 2 6))))

(document-type shape
  "A shape is the cartesian product of zero or more ranges.  Shapes can be
constructed by calling ~ or MAKE-SHAPE.  The elements of a shape are lists
of integers.  The rank of a shape is the length of these lists.  For
example, the shape (~ 0 1 ~ 1 3 ~ 3 8 4) has rank three and consists of the
integer tuples (0 1 3), (0 1 7), (0 2 3), (0 2 7).")

(document-function make-shape
  "Returns a shape whose axes are determined by the supplied list of ranges.")

(document-function shapep
  "Returns whether the supplied object is a shape."
  (shapep 42)
  (shapep (~ 1 ~ 2 ~ 3 4)))

(document-function empty-shape-p
  "Returns whether the supplied object is a shape with zero elements, i.e.,
has at least one range with size zero."
  (empty-shape-p (~ 1))
  (empty-shape-p (~ 0))
  (shapep (~ 1 ~ 2 ~ 3 3)))

(document-function shape-rank
  "Returns the rank of the supplied shape, i.e., the number of ranges it
contains."
  (shape-rank (~))
  (shape-rank (~ 1 ~ 2 ~ 3))
  (shape-rank (~ 0 9 ~ 0 9)))

(document-function shape-range
  "Returns the range denoted by the supplied SHAPE and AXIS."
  (shape-range (~ 1 ~ 2 ~ 3) 0)
  (shape-range (~ 1 ~ 2 ~ 3) 2))

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

(document-function shape=
  "Returns whether two supplied shapes denote the same set of integer tuples."
  (shape= (~) (~))
  (shape= (~ 42) (~ 42))
  (shape= (~ 1 42) (~ 1 42))
  (shape= (~ 1 42) (~ 2 42)))

(document-function shape<
  "Returns whether SHAPE1 has less elements than SHAPE2, or, if both shapes
have the same size, whether SHAPE1 has lower rank than SHAPE2, or, if both
shapes have the same rank, whether the range of SHAPE1 is smaller than the
range of SHAPE2 ranges in the lowest axis where both ranges differ in size.

The main use case for this function is to sort sequences of shapes, such
that they can be accessed in logarithmic time."
  (shape< (~ 2) (~ 3))
  (shape< (~ 3) (~ 2))
  (shape< (~ 2 ~ 4) (~ 2 ~ 2 ~ 2))
  (shape< (~ 2 ~ 2 ~ 2) (~ 2 ~ 4))
  (shape< (~ 2 ~ 2 ~ 4) (~ 2 ~ 4 ~ 2))
  (shape< (~ 2 ~ 4 ~ 2) (~ 2 ~ 2 ~ 4)))

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
  "Returns the shape whose ranges are the RANGE-INTERSECTION of each pair of
ranges of the two supplied shapes.  Signals an error if the supplied shapes
don't have the same rank."
  (shape-intersection (~ 1 11 ~ 3 14 2) (~ 1 6 ~ 1 14 3))
  (shape-intersection (~ 1 6) (~ 6 11)))

(document-function shape-intersectionp
  "Returns whether two supplied shapes have at least one common element."
  (shape-intersectionp (~ 1 6) (~ 6 10))
  (shape-intersectionp (~ 1 5) (~ 6 10)))

(document-function map-shape
  "Takes a function and a shape and applies the function to all integer
tuples of that range, in ascending order."
  (let ((l '()))
    (map-shape (lambda (i) (push i l)) (~ 1 3 ~ 3 5))
    (nreverse l)))

(document-function shape-contains
  "Returns whether the supplied shape contains the index denoted by the
supplied list of integers."
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
  "Returns a list of cons cells whose CAR is a shape and whose CDR is an
integer.  Each shape is a proper subshape of one or more of the supplied
shapes and their fusion covers all supplied shapes.  The bits of each
integer, when viewed in two's complement, encode which of the supplied
shapes are supersets of the corresponding resulting shape."
  (subdivide-shapes (list (~ 1 3 ~ 1 3) (~ 1 2 ~ 1 2)))
  (subdivide-shapes (list (~ 1 10) (~ 2 20))))

(document-function subshape
  "Returns the shape consisting of all ranges of the supplied shape in the
axes interval between the supplied start and end.  If the end argument is
not supplied, it defaults to the rank of the supplied shape."
  (subshape (~ 2 ~ 3 ~ 4) 0)
  (subshape (~ 2 ~ 3 ~ 4) 2)
  (subshape (~ 2 ~ 3 ~ 4) 1 2))

(document-function subshapep
  "Returns whether all elements of the first supplied shape are also
contained in the second supplied shape.  Signals an error if the supplied
shapes don't have the same rank."
  (subshapep (~) (~))
  (subshapep (~ 0 9) (~ 0 9))
  (subshapep (~ 0 3) (~ 1 9))
  (subshapep (~ 0 3 ~ 0 3) (~ 0 9 ~ 0 9)))

(document-function array-shape
  "Returns the shape of the supplied array.")

(document-function split-shape
  "Split the supplied SHAPE at AXIS.  The optional POSITION argument can be
supplied to describe the position at which to split.  If no POSITION
argument is supplied, split into two halves of roughly equal size.  Returns
two values, which are two shapes resulting from the split."
  (split-shape (~ 10 ~ 10) 0)
  (split-shape (~ 10 ~ 10) 1)
  (split-shape (~ 10 ~ 10) 0 3)
  (split-shape (~ 2 9 2 ~ 2 9 2) 0 3)
  (split-shape (~ 2 9 2 ~ 2 9 2) 1 3))

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
  "Returns whether a supplied transformation is invertible."
  (transformation-invertiblep (transform i j to j i))
  (transformation-invertiblep (transform i j to i)))

(document-function transformation=
  "Returns whether two supplied transformations describe the same mapping."
  (transformation=
   (transform i to (* 2 (1+ i)))
   (transform i to (+ 2 (* 2 i))))
  (transformation=
   (transform i j to i j)
   (transform i j to j i)))

(document-function transformation-similar
  "Returns whether two supplied transformations are similar.  Two
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

(document-function lazy-array
  "Returns a lazy array that is derived from the supplied object in the
following way:

- If the supplied object is a lazy array, the result is that lazy array.

- If the supplied object is a regular array, the result is a lazy array
  of the same shape, and whose indices map to the same elements.

- If the supplied object is neither a lazy array, nor a regular array, it
  is treated like an array of rank zero whose sole element is that supplied
  object.

By convention, all functions that expect an argument to be a lazy array
actually accept any kind of object and use the LAZY-ARRAY function to
convert that object to a lazy array.  Unless user code violates this
convention, scalars, regular arrays, and lazy arrays can be used
interchangeably in any argument position."
  (lazy-array #2a((1 2 3) (4 5 6)))
  (lazy-array #(1 2 3))
  (lazy-array 5)
  (lazy-array (lazy-array 5))
  (lazy-array (make-array 2 :element-type 'double-float :initial-element 0d0)))

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

(document-function lazy-broadcast-list-of-arrays
  "Returns a list of lazy arrays of the same length as the list of supplied
arrays, but where each element is broadcast such that all resulting arrays
have the same shape.  If there is no suitable broadcast shape for all
supplied arrays, an error is signaled."
  (lazy-broadcast-list-of-arrays (list #(1 2 3) 5))
  (lazy-broadcast-list-of-arrays (list #(2 3 4) #2a((1 2 3) (4 5 6)))))

(document-function lazy
  "Returns a lazy arrays whose contents are the results of applying the
supplied function element-wise to the contents of the remaining argument
arrays.  If the arguments don't agree in shape, they are first broadcast
with the function LAZY-BROADCAST-LIST-OF-ARRAYS."
  (compute (lazy #'*))
  (compute (lazy #'+ 2 3))
  (compute (lazy #'+ #(1 2) #(3 4)))
  (compute (lazy #'+ 2 #(1 2 3 4 5)))
  (compute (lazy #'* #(2 3) #2a((1 2) (3 4)))))

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
  "Returns a lazy array of integers with the shape as the supplied ARRAY,
where each array element at a certain index maps to the integer of that
index at the supplied AXIS.  If AXIS is not supplied, it defaults to zero."
  (compute (lazy-array-indices #2a((1 2) (3 4))))
  (compute (lazy-array-indices #2a((1 2) (3 4)) 1))
  (compute (lazy-array-indices "abc")))

(document-function lazy-shape-indices
  "Returns a lazy array of integers of the shape of SHAPE, where each array
element at a certain index maps to the integer of that index at the supplied AXIS.
If AXIS is not supplied, it defaults to zero."
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
  "The primary interface for evaluating lazy arrays.  It takes any number of
arguments that must be lazy arrays or objects that can be converted to lazy
arrays with the function LAZY-ARRAY --- and returns the same number of
possibly specialized regular arrays with the corresponding contents.  A
special case is that whenever an array with rank zero would be returned, it
instead returns the sole element of that array.  The reason for this
treatment of scalars is that Petalisp treats every object as an array,
whereas in Common Lisp the rank zero array containing an object and that
object are distinct entities.  Of those two distinct representations, the
non-array one is much more intuitive and useful in practice, so this is the
one being returned.

Whenever a shape of any of the supplied arrays is not a valid shape for a
regular array, that array is collapsed before being computed, i.e., each
axis is shifted to begin with zero, and divided by the step size.

All the heavy lifting in Petalisp happens within this function.  The exact
details of how it operates aren't important for application programmers,
but it is valuable to understand the rough steps that happen internally.
The individual steps it performs are:

1. Convert each supplied argument to a lazy array.

2. Collapse each lazy array whose shape isn't equivalent to that of a
   regular array.

3. Determine the dependency graph whose roots are the collapsed lazy
   arrays, and whose leaves are lazy arrays resulting from a call to
   LAZY-ARRAY on a regular array or scalar.

4. Optimize the dependency graph, discard all unused parts, and plan a
   schedule that is fast and has reasonable memory requirements.

5. Execute the schedule on the available hardware.  Make use of CPUs, GPUs,
   or even distributed systems where possible. Gather the results in the
   form of regular arrays.

6. Change the internal representation of all the originally supplied lazy
   arrays such that future calculations involving them directly use the
   computed results.

7. Return the results as multiple values, while replacing any array with
   rank zero with the single element contained in that array.

This function is the workhorse of Petalisp.  A lot of effort went into
making it not only powerful, but also extremely fast.  The overhead of
assembling a graph of lazy arrays and passing it to COMPUTE instead of
invoking an already compiled and optimized imperative program is usually on
the order of just a few microseconds."
  (compute (lazy-array #(1 2 3)))
  (compute #(1 2 3))
  (compute 5)
  (compute #0a42)
  (compute #(1 2 3) 5 #0a42))

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

(document-function compute-asynchronously
  "Hints that it would be worthwhile to compute the supplied arrays
asynchronously.  Returns a request object that can be passed to the
functions WAIT and COMPLETEDP.")

(document-function evaluator
  "Returns a function that can be applied to arrays to achieve the effect
described by the supplied UNKNOWNS and ARRAYS.")

(document-function wait
  "Blocks until the work designated by some COMPUTE-ASYNCHRONOUSLY operations
has been completed.  Each argument must be a request returned by
COMPUTE-ASYNCHRONOUSLY.")

(document-function completedp
  "Returns whether the work designated by some COMPUTE-ASYNCHRONOUSLY
operations has been completed.  Each argument must be a requests returned
by COMPUTE-ASYNCHRONOUSLY.")
