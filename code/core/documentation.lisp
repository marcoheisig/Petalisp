;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(document-type range
  "A range denotes a set of integers, starting from a lower bound START,
by a fixed stride STEP, to an inclusive upper bound END.")

(document-function range
  "Returns a new, normalized range from the supplied parameters.  Can be
invoked with one, two or three integers.  In the case of a single integer,
it constructs the range with exactly that element.  In the case of two
integers, it creates a contiguous range.  In the case of three arguments,
the first and the third argument denote the interval, and the second
argument denotes the step size.  If the first argument and the last
argument are not congruent modulo the step size, the latter one is moved
towards the former until they are.

Examples:

 (range 5)
  => #<range 5>

 (range 5 9)
  => #<range 5 ... 9>

 (range 5 2 13)
  => #<range 5 7 ... 13>

 (range 7 3 -3)
  => #<range -2 1 4 7>
")

(document-function size-one-range-p
  "Checks whether the supplied range has a size of one.")

(document-function split-range
  "Splits the supplied range R into a lower and an upper half and returns
them as multiple values.  In case R has an odd number of element, the lower
half will have one more element than the upper half.

An error is signaled if the supplied range has only a single element.
")

(document-function map-range
  "Takes a function and a range and applies the function to all integers of
that range, in ascending order.")

(document-function range-equal
  "Check whether two supplied ranges describe the same set of integers.")

(document-function range-contains
  "Check whether the supplied range contains a particular integer.")

(document-function range-intersection
  "Returns the range containing exactly those elements that occur in both
supplied ranges.  Returns NIL if there are no such elements.

Examples:

 (range-intersection (range 1 10) (range 2 20))
  => #<range 2 ... 10>

 (range-intersection (range 3 2 13) (range 1 3 13))
  => #<range 7 13>
")

(document-function range-intersectionp
  "Check whether two supplied ranges have at least one common element.")

(document-function range-difference-list
  "Compute the difference of two ranges R1 and R2.  Returns a list of
disjoint subranges of R1 that describe exactly those integers appearing in
R1 but not in R2.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shapes

(document-variable ~
  "The symbol ~ is self-evaluating.  Its only purpose is to separate range
designators in the function named ~.")

(document-function ~
  "Construct a shape from zero or more tilde-separated range designators.

Examples:

 (~)
  => (~)

 (~ 1 2 3)
  => (~ 1 2 3)

 (~ 0 2 9 ~ 0 2 9)
  => (~ 0 2 8 ~ 0 2 8)

 (apply #'~ 1 9 (loop repeat 3 append '(~ 2 5)))
  => (~ 1 9 ~ 2 5 ~ 2 5 ~ 2 5)
")

(document-type shape
  "A shape is the cartesian product of zero or more ranges.  Shapes can be
constructed by calling ~ or MAKE-SHAPE.  The elements of a shape are lists
of integers.  The rank of a shape is the length of these lists.  For
example, the shape (~ 0 ~ 1 2 ~ 3 4 7) has rank three and consists of the
integer tuples (0 1 3), (0 1 7), (0 2 3), (0 2 7).")

(document-function make-shape
  "Constructs a shape from a supplied list of ranges.")

(document-function shape-size
  "Returns that number of integer tuples denoted by the supplied shape.")

(document-function shape-equal
  "Check whether two supplied shapes denote the same set of integer tuples.")

(document-function shape-difference-list
  "Compute the difference of two shapes S1 and S2.  Returns a list of
disjoint subshapes of S1 that describe exactly those integer tuples
appearing in S1 but not in S2.")

(document-function shape-intersection
  "Returns the shape containing exactly those integer tuples that occur in
both supplied shapes.  Returns NIL if there are no such elements.

Examples:

 (shape-intersection (~ 1 10 ~ 3 2 13) (~ 1 5 ~ 1 3 13))
  => (~ 1 5 ~ 7 6 13)

 (shape-intersection (~ 1 5) (~ 6 10))
  => nil
")

(document-function shape-intersectionp
  "Check whether two supplied shapes have at least one common element.")

(document-function map-shape
  "Takes a function and a shape and applies the function to all integer
tuples of that range, in ascending order.")

(document-function shape-contains
  "Check whether the supplied shape contains a particular integer tuple.

Examples:

 (shape-contains (~ 1 9) (list 4))
  => t

 (shape-contains (~ 1 2 9) (list 4))
  => nil
")

(document-function enlarge-shape
  "For a given shape S and range R, this function returns a shape whose
  first range is R, and whose remaining ranges are those of S.

Examples:
 (enlarge-shape (~) (range 1 10))
  => (~ 1 10)

 (enlarge-shape (~ 1 3) (range 1 4))
  => (~ 1 4 ~ 1 3)
")

(document-function shrink-shape
  "This function expects a single shape with one or more ranges R1 to Rn.
It returns a shape with the ranges R2 to R1, and, as a second value, the
range R1 that has been peeled off.

Examples:

 (shrink-shape (~ 1 10))
  => (~)
  => #<range 1 ... 10>

 (shrink-shape (~ 1 10 ~ 0 2))
  => (~ 0 2)
  => #<range 1 ... 10>
")

(document-function subdivision
  "Returns a list of disjoint shapes. Each resulting shape is a proper
subshape of one or more of the arguments and their fusion covers all
arguments.

Examples:

 (subdivision (list (~ 1 2 ~ 1 2) (~ 1 ~ 1)))
  => ((~ 1 ~ 1) (~ 2 ~ 1 2) (~ 1 ~ 2))

 (subdivision (list (~ 1 10) (~ 2 20)))
  => ((~ 11 20) (~ 2 10) (~ 1))
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy Arrays

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
applying the transformation to the index while retaining the value.

Examples:

 ;; Broadcasting.
 (compute (reshape 4 (~ 0 4)))
  => #(4 4 4 4 4)

 ;; Selection.
 (compute (reshape #(1 2 3 4) (~ 1 2)))
  => #(2 3)

 ;; Lexicographic reordering.
 (compute (reshape (indices (~ 1 9)) (~ 0 2 ~ 0 2)))
  => #2A((1 2 3) (4 5 6) (7 8 9))

 ;; Element-wise transformation.
 (compute (reshape #2A((1 2) (3 4)) (τ (i j) (j i))))
  => #2A((1 3) (2 4))

 ;; Multiple modifications at once.
 (compute (reshape #(1 2 3 4) (~ 1 2) (~ 0 1 ~ 0 1)))
  => #2A((2 3) (2 3))
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
