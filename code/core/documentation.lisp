;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

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
;;; Transformations

(document-type transformation
  "A transformation with input rank N and output rank M is a mapping
from lists of length N to lists of rank M.")

(document-type identity-transformation
  "An identity transformation of rank N maps every list of length N to
itself.  An identity transformation is its own inverse.")

(document-type hairy-transformation
  "A hairy transformation is a combination of any number of permutations,
affine-linear mappings, and removal of insertions of one-element ranks.
Furthermore, the inputs of a hairy transformation can be subject to integer
constraints.")

(document-function transformation-invertiblep
  "Check whether a supplied transformation is invertible."
  (transformation-invertiblep (τ (i j) (j i)))
  (transformation-invertiblep (τ (i j) (i))))

(document-function transformation-equal
  "Check whether two supplied transformations describe the same mapping."
  (transformation-equal
   (τ (i) ((* 2 (1+ i))))
   (τ (i) ((+ 2 (* 2 i)))))
  (transformation-equal
   (τ (i j) (i j))
   (τ (i j) (j i))))

(document-function transformation-similar
  "Check whether two supplied transformations are similar.  Two
transformations are similar if they have the same permutation, the same
inputs constraints, the same scalings, and offsets whose entries differ in
at most DELTA."
  (transformation-similar
   (τ (a) (a))
   (τ (a) ((1+ a)))
   0)
  (transformation-similar
   (τ (a) (a))
   (τ (a) ((1+ a)))
   1)
  (transformation-similar
   (τ (i j) ((+ j 2) i))
   (τ (i j) ((- j 1) i))
   2)
  (transformation-similar
   (τ (i j) ((+ j 2) i))
   (τ (i j) ((- j 1) i))
   3))

(document-function compose-transformations
  "Returns a single transformation that is equivalent to consecutive
invocations of the supplied transformations in right-to-left order."
  (compose-transformations
   (τ (i) ((* 2 (1+ i))))
   (τ (i) ((1- (/ i 2)))))
  (compose-transformations
   (τ (i j) ((+ i 5) (+ j 7)))
   (τ (i j) ((* j 2) (* i 3)))))

(document-function invert-transformation
  "Returns the inverse of the supplied transformation.

An error is signaled if the supplied transformation is not invertible."
  (invert-transformation
   (τ (i) ((+ 2 i))))
  (invert-transformation
   (τ (a b) ((+ (* 2 b) 5) (+ (* 3 a) 7))))
  (invert-transformation
   (τ (a 0) (a)))
  (invert-transformation
   (τ (a b) (a))))

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

(document-function transform
  "Reorder, scale and shift the given OBJECT according to TRANSFORMATION.")

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

(document-type immediate
  "An immediate is a lazy array whose values do not depend on any other
lazy array.")

(document-type non-immediate
  "A non-immediate is an array with one or more inputs, i.e., other lazy
arrays upon which its values depend.")

(document-type empty-array
  "The empty array conveys no information except that it is empty.  Certain
operations, for example, reductions of a scalar yield an empty array.  Most
functions that work on lazy arrays will handle empty arrays in a sensible
way.")

(document-type non-empty-array
  "A non-empty lazy array has a shape and knowledge about its element
type.")

(document-type array-immediate
  "An array immediate is a lazy array whose shape, element type and
contents are the those of a supplied Common Lisp array.")

(document-type range-immediate
  "A range immediate is a rank one array, whose values are equal to its indices.")

