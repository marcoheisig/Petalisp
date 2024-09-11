;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

(document-function range
  "Returns a new, normalized range from the supplied parameters.
This function can be invoked with one, two or three integers.  If it is called
with a single argument, the result is a range starting from zero, with step
size one, up to but excluding the supplied argument.  If the range constructor
is called with two arguments, the result is still a range with a step size
of one, but with the first argument as the inclusive lower bound, and with the
second argument as the exclusive upper bound.  The three argument version
behaves just like the two argument version, except that the additional third
argument denotes the step size.  The sign of the step size gives the direction
of the range: If the sign is positive, then the exclusive upper bound must be
larger than the inclusive lower bound or the resulting range is empty.  If the
sign is negative, the first argument is used as an inclusive upper bound, and
the second argument is used as an exclusive lower bound."
  (range 5)
  (range 5 9)
  (range 5 13 2)
  (range 1 7 -2)
  (range 7 1 -2))

(document-function rangep
  "Returns whether the supplied object is a range."
  (rangep 42)
  (rangep (range 1 3 2)))

(document-function range-emptyp
  "Returns whether a supplied range has zero elements."
  (range-emptyp (range 0))
  (range-emptyp (range 1)))

(document-function range-with-size-one-p
  "Returns whether the supplied range has a size of one."
  (range-with-size-one-p (range 5))
  (range-with-size-one-p (range 5 7 2))
  (range-with-size-one-p (range 5 7 3)))

(document-function range-size
  "Returns the number of elements in the supplied range.")

(document-function range-start
  "Returns the lowest integer contained in the supplied range.  Signals an
error in case the range has zero elements.")

(document-function range-step
  "Returns the difference between any two successive integers in the supplied
range.  Signals an error in case the range has zero elements.")

(document-function range-last
  "Returns the highest integer contained in the supplied range.
Signals an error in case the range has zero elements.")

(document-function range-end
  "Returns an integer that is larger than any integer in the supplied
range by at most its step size.  An error is signaled in case the range has
zero elements.")

(document-function split-range
  "Splits the supplied range R into a lower and an upper half and returns
those two halves as multiple values.  In case R has an odd number of elements,
the lower half will have one more element than the upper half.  The optional
POSITION argument is a real number that can be used to prescribe the point at
which to split the range."
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
supplied ranges."
  (range-intersection (range 1 10) (range 2 20))
  (range-intersection (range 3 14 2) (range 1 14 3)))

(document-function range-intersectionp
  "Returns whether two supplied ranges have at least one common element."
  (range-intersectionp (range 1 10) (range 2 20))
  (range-intersectionp (range 0 7 2) (range 1 8 2)))

(document-function range-difference-list
  "Returns a list of disjoint subranges of RANGE1 that describe exactly those
integers appearing in RANGE1 but not in RANGE2.")

(document-function subrangep
  "Returns whether all elements of the first supplied range are contained in the
second supplied range."
  (subrangep (range 0 10 2) (range 0 20 2))
  (subrangep (range 0) (range 0))
  (subrangep (range 10) (range 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shapes

(document-variables (~ ~*)
  "The symbols ~, ~* are self-evaluating.  Their only purpose is to separate
range designators in the functions named ~, ~*.")

(document-function ~
  "Returns a shape whose ranges are derived by processing each occurrence of
one of the self-evaluating delimiter symbols ~ and ~*, and the arguments
following such a delimiter up to the next one.  Each such group contributes
one or more ranges to the resulting shape.  The behavior of each delimiter
is as follows:

- The ~ delimiter must be followed by one, two, or three integers that are
  then supplied to the RANGE function to construct the single resulting
  range.

- The ~* delimiter must be followed by any number of ranges or lists of ranges
  that are incorporated into the resulting shape in the order that they appear."
  (~ 8)
  (~ 1 10)
  (~ 0 10 2 ~ 0 10 2)
  (apply #'~ 1 10 '(~ 2 6 ~ 2 6)))

(document-function ~*
  "Returns a shape whose ranges are derived by processing each occurrence of
one of the self-evaluating delimiter symbols ~ and ~* in the same manner as the
function ~."
  (~*)
  (~* (range 1 10) (range 2 9) ~ 42)
  (~* ~ 10)
  (~* (make-list 4 :initial-element (range 3))))

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

(document-function shape-emptyp
  "Returns whether the supplied object is a shape with zero elements, i.e.,
has at least one range with size zero."
  (shape-emptyp (~ 1))
  (shape-emptyp (~ 0))
  (shape-emptyp (~ 1 ~ 2 ~ 3 3)))

(document-function shape-with-size-one-p
  "Returns whether the supplied shape has a size of one."
  (shape-with-size-one-p (~*))
  (shape-with-size-one-p (~ 1))
  (shape-with-size-one-p (~ 2)))

(document-function shape-rank
  "Returns the rank of the supplied shape, i.e., the number of ranges it
contains."
  (shape-rank (~*))
  (shape-rank (~ 1 ~ 2 ~ 3))
  (shape-rank (~ 0 9 ~ 0 9)))

(document-function shape-range
  "Returns the range denoted by the supplied SHAPE and AXIS."
  (shape-range (~ 1 ~ 2 ~ 3) 0)
  (shape-range (~ 1 ~ 2 ~ 3) 2))

(document-function shape-ranges
  "Returns a list of all ranges contained in the supplied shape."
  (shape-ranges (~*))
  (shape-ranges (~ 1 ~ 2 ~ 3))
  (shape-ranges (~ 0 9 ~ 0 9)))

(document-function shape-dimension
  "Returns the size of the shape's range in a particular axis"
  (shape-dimension (~ 2 ~ 5) 0)
  (shape-dimension (~ 2 ~ 5) 1)
  (shape-dimension (~ 10 30 2) 0))

(document-function shape-dimensions
  "Returns a list of the sizes of each range of the supplied shape."
  (shape-dimensions (~*))
  (shape-dimensions (~ 0 9))
  (shape-dimensions (~ 1 9))
  (shape-dimensions (~ 0 2 9))
  (shape-dimensions (~ 0 4 ~ 0 5 ~ 0 6)))

(document-function shape-size
  "Returns that number of integer tuples denoted by the supplied shape."
  (shape-size (~*))
  (shape-size (~ 2 9))
  (shape-size (~ 1 9 ~ 1 8)))

(document-function shape=
  "Returns whether two supplied shapes have the same rank and ranges."
  (shape= (~*) (~*))
  (shape= (~ 42) (~ 42))
  (shape= (~ 1 42) (~ 1 42))
  (shape= (~ 1 42) (~ 2 42)))

(document-function shape<
  "Returns whether SHAPE1 has less elements than SHAPE2, and if both shapes
have the same size, whether SHAPE1 has lower rank than SHAPE2, and if both
shapes have the same rank, whether the range of SHAPE1 is smaller than the
range of SHAPE2 ranges in the lowest axis where both ranges differ in size.

The main use case for this function is to sort sequences of shapes to obtain a
canonical ordering."
  (shape< (~ 2) (~ 3))
  (shape< (~ 3) (~ 2))
  (shape< (~ 2 ~ 4) (~ 2 ~ 2 ~ 2))
  (shape< (~ 2 ~ 2 ~ 2) (~ 2 ~ 4))
  (shape< (~ 2 ~ 2 ~ 4) (~ 2 ~ 4 ~ 2))
  (shape< (~ 2 ~ 4 ~ 2) (~ 2 ~ 2 ~ 4)))

(document-function shape-difference-list
  "Computes the difference of two shapes S1 and S2.  Returns a list of
disjoint subshapes of S1 that describe exactly those indices appearing in S1
but not in S2."
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
  "Returns whether two supplied shapes have at least one common index."
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
supplied list of integers.  Signals an error if the length of the list of
integers differs from the shape's rank."
  (shape-contains (~ 1 9) (list 4))
  (shape-contains (~ 1 2 9) (list 4)))

(document-function petalisp.core:shrink-shape
  "This function expects a single shape with one or more ranges R1 to Rn.
It returns a shape with the ranges R2 to R1, and, as a second value, the
range R1 that has been peeled off."
  (petalisp.core:shrink-shape (~ 1 10))
  (petalisp.core:shrink-shape (~ 1 10 ~ 0 2)))

(document-function petalisp.core:enlarge-shape
  "For a given shape S and range R, this function returns a shape whose
  first range is R, and whose remaining ranges are those of S."
  (petalisp.core:enlarge-shape (~*) (range 1 10))
  (petalisp.core:enlarge-shape (~ 1 3) (range 1 4)))

(document-function petalisp.core:subdivide-arrays
  "Invoke SUBDIVIDE-SHAPES on the shapes of the supplied ARRAYS."
  (petalisp.core:subdivide-arrays (list))
  (petalisp.core:subdivide-arrays (list #()))
  (petalisp.core:subdivide-arrays (list #() #()))
  (petalisp.core:subdivide-arrays (list #(1 2 3 4) #(1 2))))

(document-function subdivide-shapes
  "Returns a list of cons cells whose CAR is a shape and whose CDR is an
integer.  Each shape is a proper subshape of one or more of the supplied shapes
and the fusion of all these shapes covers all the supplied shapes.  The bits of
each integer, when viewed in two's complement, encode which of the supplied
shapes are supersets of that particular resulting shape."
  (subdivide-shapes (list (~ 1 10) (~ 2 20)))
  (subdivide-shapes (list (~ 1 3 ~ 1 3) (~ 1 2 ~ 1 2))))

(document-function petalisp.core:shape-subseq
  "Returns the shape consisting of all ranges of the supplied shape in the
axes interval between the supplied start and end.  If the end argument is
not supplied, it defaults to the rank of the supplied shape."
  (petalisp.core:shape-subseq (~ 2 ~ 3 ~ 4) 0)
  (petalisp.core:shape-subseq (~ 2 ~ 3 ~ 4) 2)
  (petalisp.core:shape-subseq (~ 2 ~ 3 ~ 4) 1 2))

(document-function petalisp.core:shape-prefix
  "Returns the shape that consists of the lower axes of the supplied first
argument, and whose rank is given by the second argument."
  (petalisp.core:shape-prefix (~ 1 ~ 2  ~ 3) 0)
  (petalisp.core:shape-prefix (~ 1 ~ 2  ~ 3) 1)
  (petalisp.core:shape-prefix (~ 1 ~ 2  ~ 3) 2))

(document-function petalisp.core:shape-suffix
  "Returns the shape that consists of the higher axes of the supplied first
argument, and whose rank is given by the second argument."
  (petalisp.core:shape-suffix (~ 1 ~ 2  ~ 3) 0)
  (petalisp.core:shape-suffix (~ 1 ~ 2  ~ 3) 1)
  (petalisp.core:shape-suffix (~ 1 ~ 2  ~ 3) 2))

(document-function subshapep
  "Returns whether all elements of the first supplied shape are also
contained in the second supplied shape.  Signals an error if the supplied
shapes don't have the same rank."
  (subshapep (~*) (~*))
  (subshapep (~ 0 9) (~ 0 9))
  (subshapep (~ 0 3) (~ 1 9))
  (subshapep (~ 0 3 ~ 0 3) (~ 0 9 ~ 0 9)))

(document-function petalisp.core:array-shape
  "Returns the shape of the supplied array.")

(document-function shape-designator-shape
  "Returns the shape of the supplied shape designator.  If the designator
is already a shape, the result is that shape.  If the designator is a
regular array or lazy array, the result is the shape of that array.  If the
result is any other object, the result is a shape with rank zero.")

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
  "Returns a transformation from the given inputs to the given outputs.
Inputs and outputs are separated by the symbol PETALISP:TO.  Each input can
either be a symbol or an integer.  If the input is a symbol, it is the name
under which the value of that input can be referenced in one of the outputs.
If the input is a integer, it denotes an input constraint meaning that it is an
error to later apply that transformation to an index that differs from that
constraint in that position.

Each output is an arbitrary form that may reference up to one of the input
variables.  This form is then evaluated repeatedly in a context where the
referenced input variable is bound to a different integer, to determine the
coefficients for the linear mapping from the referenced input to the output.
Signals an error if any output form references more than one input, returns
anything other than an integer, or describes a mapping that is not linear."
  (transform i to (+ i 1))
  (transform i to (+ (+ i 1) 5))
  (transform 1 2 to 3)
  (transform i j to j i))

(document-type transformation
  "A transformation with input rank N and output rank M is a mapping
from lists of length N to lists of rank M.")

(document-type petalisp.core:identity-transformation
  "An identity transformation of rank N maps every list of length N to
itself.  An identity transformation is its own inverse.")

(document-function transformationp
  "Returns whether the supplied object is a transformation."
  (transformationp (transform i j to j i))
  (transformationp (transform i j to i j))
  (transformationp 42))

(document-function petalisp.core:identity-transformation-p
  "Returns whether the supplied object is an identity transformation."
  (petalisp.core:identity-transformation-p (transform i j to j i))
  (petalisp.core:identity-transformation-p (transform i j to i j)))

(document-function transformation-identityp
  "Returns whether a supplied transformation is an identity transformation."
  (transformation-identityp (transform i j to j i))
  (transformation-identityp (transform i j to i j)))

(document-function transformation-input-rank
  "Returns the rank that any shape that can be transformed with this
transformation must have.")

(document-function transformation-input-mask
  "Returns a vector with one element per axis of any possible input.  Each
element of this vector is either an integer, meaning the index component of
any input index in the corresponding axis must be that integer, or NIL,
meaning that the index component can be any integer.")

(document-function transformation-output-rank
  "Returns the rank that shapes created by applying this transformation will
have.")

(document-function transformation-output-mask
  "Returns a vector with one element per axis of any possible output.  Each
element of this vector is either an integer that is the corresponding input
axis that is referenced by this output axis, or NIL, if and only if the
scaling of that output axis is zero.")

(document-function transformation-scalings
  "Returns a vector with one element per axis of any possible output.  Each
element of this vector is a rational number that is multiplied with the
input index component indicated by the corresponding output mask entry
before it is added to the corresponding offset.")

(document-function transformation-offsets
  "Returns a vector with one element per axis of any possible output.  Each
element of this vector is a rational number that is added to the
input index component indicated by the corresponding output mask entry
after it is multiplied with the corresponding scaling.")

(document-function transformation-invertiblep
  "Returns whether the supplied object is an invertible transformation."
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

(document-function petalisp.core:transformation-similar
  "Returns whether two supplied transformations are similar.  Two
transformations are similar if they have the same permutation, the same
inputs constraints, the same scalings, and offsets whose entries differ in
at most DELTA."
  (petalisp.core:transformation-similar
   (transform a to a)
   (transform a to (1+ a))
   0)
  (petalisp.core:transformation-similar
   (transform a to a)
   (transform a to (1+ a))
   1)
  (petalisp.core:transformation-similar
   (transform i j to (+ j 2) i)
   (transform i j to (- j 1) i)
   2)
  (petalisp.core:transformation-similar
   (transform i j to (+ j 2) i)
   (transform i j to (- j 1) i)
   3))

(document-function compose-transformations
  "Returns a single transformation that is equivalent to consecutive
invocations of the supplied transformations in right-to-left order."
  (compose-transformations
   (transform i to (/ i 2))
   (transform i to (+ i 2))
   (transform i to (* i 4)))
  (compose-transformations
   (transform i to (* 2 (1+ i)))
   (transform i to (1- (/ i 2))))
  (compose-transformations
   (transform i j to (+ i 5) (+ j 7))
   (transform i j to (* j 2) (* i 3))))

(document-function invert-transformation
  "Returns the inverse of the supplied transformation, or signals an error if the
supplied transformation is not invertible."
  (invert-transformation
   (transform i to (+ 2 i)))
  (invert-transformation
   (transform a b to (+ (* 2 b) 5) (+ (* 3 a) 7)))
  (invert-transformation
   (transform a 0 to a))
  (handler-case
      (invert-transformation
       (transform a b to a))
    (error () :error)))

(document-function petalisp.core:map-transformation-inputs
  "For each input of TRANSFORMATION, invoke FUNCTION with the input index
and the corresponding input constraint, or null, if there is no input
constraint for this input.

If FROM-END is false, the input indices are traversed in ascending order.
Otherwise, they are traversed in descending order.")

(document-function petalisp.core:map-transformation-outputs
  "For each output of TRANSFORMATION, invoke FUNCTION with the output
index, input index, the scaling and the offset of that output.

An input index of NIL and a scaling of zero is used, if (and only if) the
output is constant.

If FROM-END is false, the output indices are traversed in ascending order.
Otherwise, they are traversed in descending order.")

(document-function petalisp.core:enlarge-transformation
  "Given a transformation mapping from (i1 ... iN) to (j1 ... jM),
return a transformation mapping from (i0 i1 ... iN iN+1) to
((+(* i0 SCALE) OFFSET) j1 ... jM).")

(document-function transform-shape
  "Returns the shape that describes the set of all results of applying the
supplied transformation to each index of the supplied shape."
  (transform-shape (~ 2 ~ 3) (transform i j to j i))
  (transform-shape (~ 10) (transform i to (1+ (* 2 i)))))

(document-function transform-index
  "Returns the index that results from applying the supplied transformation to
the supplied index."
  (transform-index '(10) (transform i to (+ i 2)))
  (transform-index '(1 2 3) (transform i j k to j (* 3 i) (1+ k))))

(document-function petalisp.core:identity-transformation
  "Returns an identity transformation of the specified rank.")

(document-function make-transformation
  "Returns a transformation that is created according to the supplied keyword
arguments.  Valid keyword arguments are:

- :INPUT-RANK A non-negative integer that is the rank of any permissible index
  or shape supplied to this transformation.  Defaults to the length of the
  supplied input mask, or, if no input mask is supplied, to the value of the
  output rank.  Signals an error if neither the input rank nor the output rank
  could be inferred.

- :OUTPUT-RANK A non-negative integer that is the rank of any possible index or
  shape resulting from this transformation.  Defaults to the length of the
  supplied scalings, offsets, or output mask, or, if none of these are
  supplied, to the value of the input rank.  Signals an error if neither the
  input rank nor the output rank could be inferred.

- :INPUT-MASK A sequence with one element per axis of the transformation's
  input. Each element must either be an integer, in which case only that
  integer may occur in the corresponding axis of the input, or NIL, in which
  case any integer may occur in the corresponding axis.

- :OUTPUT-MASK A sequence with one element per axis of the transformation's
  output.  Each element must either be an integer, in which case this integer
  denotes the axis of the input that is to be scaled, shifted and sent to the
  current position's output, or NIL, in which case only the corresponding
  offset value is sent to the current output.  This way, the output mask can
  encode both permutations of the input, as well as insertion and removal of
  axes.  If this keyword argument is not supplied, it defaults to a sequence of
  consecutive integers from zero to right below the minimum of the input rank
  and the output rank, followed by entries of NIL in case the output rank
  exceeds the input rank.

- :SCALINGS A sequence with one element per axis of the transformation's output
  whose elements are rational numbers.  Each integer of a transformation's
  output is computed by multiplying the input denoted by the output mask with
  its corresponding entry in this sequence and adding to the corresponding
  offset.  If an output mask entry is NIL, the corresponding scaling is ignored
  and the offset of that output axis is returned as is.  If this keyword
  argument is not supplied, it defaults to a sequence of ones.

- :OFFSETS A sequence with one element per axis of the transformation's output.
  Each element must be a rational number that is added to the corresponding
  output value after scaling has taken place.  If this keyword argument is not
  supplied, it defaults to a sequence of zeros.

Signals an error if some of the sequences supplied as :OUTPUT-MASK, :SCALINGS,
or :OFFSETS differ in length."
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

(document-function lazy-array-p
  "Returns whether the supplied object is a lazy array."
  (lazy-array-p (lazy #'* 21 2))
  (lazy-array-p 42)
  (lazy-array-p #2a((1 2) (3 4))))

(document-function lazy-array-shape
  "Returns the shape of the supplied lazy array.")

(document-function lazy-array-element-type
  "Returns the element of the supplied lazy array.
The element type is a conservative upper bound on the types of all the elements
in that lazy array.  It is derived automatically during lazy array
construction.  When computing a lazy array, the resulting regular array's
element type is the upgraded array element type of the lazy arrays element
type.")

(document-function lazy-array-rank
  "Returns the rank of the supplied lazy array.
The rank of a lazy array is the number of ranges that constitute its shape.")

(document-function lazy-array-size
  "Returns the number of elements of the supplied lazy array.")

(document-function lazy-array-dimension
  "Returns the number of elements of the supplied lazy array along a particular
axis.")

(document-function lazy-array-dimensions
  "Returns a list that consists of the number of elements of the supplied lazy
array along each of its axes.")

(document-function lazy-array-range
  "Returns the range of the supplied lazy array's shape along a particular axis.")

(document-function lazy-array-ranges
  "Returns the list of all ranges that constitute the supplied lazy array's
 shape.")

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
  (lazy-array (make-array 64 :element-type 'bit :initial-element 1)))

(document-function lazy-reshape
  "Returns the lazy array that is obtained by successively reshaping the
supplied array with the supplied modifiers in left-to-right order.  There are
three kinds of modifiers: Transformations that describe a reordering of values,
functions which are applied to the shape of the lazy array to obtain additional
modifiers, and shape designators that describe a selection, move, or
broadcasting of values.

More precisely, the processing maintains a lazy array that is initialized to
the result of applying LAZY-ARRAY constructor to the supplied first argument,
and which is successively updated it with the result of applying one modifier
at a time according to the following rules:

1. If the modifier is an invertible transformation, reorder the elements of
   that array according to that transformation.  If the lazy array has lower
   rank than expected from the transformation, broadcast it first to that rank.
   If the lazy array has higher rank than expected from the transformation,
   leave these extra axes as they are and later append them to the shape of the
   resulting lazy array.

2. If the modifier is a function, apply it to the shape of the lazy array to
   obtain a number of new modifiers as multiple values.  Process the new
   modifiers as if they were supplied instead of this function modifier.

3. If the modifier is a shape designator then move, broadcast or sample each
   axis of the lazy array to match that shape.  If the lazy array has lower
   rank than the designated shape, first broadcast it to that rank.  If the
   lazy array has higher rank than the designated shape, leave the remaining
   axes as they are and later append them to the shape of the resulting lazy
   array. For each axis, derive the mapping from the range of the lazy array to
   the corresponding range of designated shape according to the following
   rules:

   a) If both the source range and the target range have the same size, move
      the elements of that axis so that they end up on the target range while
      maintaining the original order.

   b) If the source range has a size of one, broadcast it to the target range
      of that axis.

   c) If the target range is a proper subset of the source range, select only
      those elements of that axis that fall within the target range."
  (compute (lazy-reshape #(1 2 3 4) (~ 1 2)))
  (compute (lazy-reshape #(1 2 3 4) (~ 2 ~ 3)))
  (compute (lazy-reshape #(1 2 3 4) (~ 4 ~ 2)))
  (compute (lazy-reshape #2A((1 2) (3 4)) (transform i j to j i)))
  (compute (lazy-reshape #(1 2 3 4) (transform i to (- i))))
  (compute (lazy-reshape #(1 2 3 4) (transform i to (- i)) (~ -2 0)))
  (compute (lazy-reshape #(1 2 3 4) (lambda (s) (~ 1 (1- (shape-size s)))))))

(document-function broadcast
  "Returns a list of lazy arrays that all have the same shape, where each lazy array is
a broadcasting reference to the corresponding element of the supplied list of
arrays or scalars.  As a second value, returns the shape of all the resulting
lazy arrays.  Signals an error if there is no suitable shape to which all the
supplied arrays can be broadcast.  The resulting shape is constructed according
to the following rules:

1. The rank of the resulting shape is the maximum of the rank of all the
   supplied arrays.

2. Each range of any of the supplied arrays must either have a size of one, in
   which case it is broadcast to the to the corresponding range of the
   resulting shape, or it must be equal to the corresponding range of the
   resulting shape.

3. The missing axes of lazy arrays that have a lower rank than the resulting
   shape are broadcast as if they had a range of size one.

4. In any axis in which all supplied arrays have a range with size one, the
   resulting shape has the range of the leftmost supplied array."
  (broadcast (list #(1 2 3) 5))
  (broadcast (list #2A((1 2 3)) #2A((4) (5))))
  (apply #'compute (broadcast (list #(1 2 3) 5)))
  (apply #'compute (broadcast (list #2a((1 2) (3 4)) #(7 8)))))

(document-function lazy
  "Returns a lazy array whose contents are the results of applying the
supplied function element-wise to the contents of the remaining argument
arrays.  If the arguments don't agree in shape, they are first broadcast
with the function BROADCAST."
  (compute (lazy #'*))
  (compute (lazy #'+ 2 3))
  (compute (lazy #'+ #(1 2) #(3 4)))
  (compute (lazy #'+ 2 #(1 2 3 4 5)))
  (compute (lazy #'* #(2 3) #2a((1 2) (3 4)))))

(document-function lazy-multiple-value
  "Returns as many lazy arrays as indicated by the integer that is the first
supplied argument, whose contents are the results of applying the function that
is the second supplied argument element-wise to the contents of the remaining
argument arrays.  If the arguments don't agree in shape, they are first
broadcast together."
  (multiple-value-call #'compute (lazy-multiple-value 0 #'*))
  (multiple-value-call #'compute (lazy-multiple-value 1 #'*))
  (multiple-value-call #'compute (lazy-multiple-value 2 #'*))
  (multiple-value-call #'compute (lazy-multiple-value 2 #'floor #(2 3) #2a((1 2) (3 4))))
  (multiple-value-call #'compute (lazy-multiple-value 3 #'values-list #((1 2 3) (4 5 6)))))

(document-function lazy-fuse
  "Returns a lazy array that is a combination of all the values of all the
supplied arrays.  Signals an error if any of the supplied arrays overlap,
don't have the same rank, or if the union of all their shapes cannot be
represented as a shape."
  (compute (lazy-fuse (lazy-reshape 1 (~ 0 2))
                      (lazy-reshape 2 (~ 2 4))))
  (compute (lazy-fuse (lazy-reshape 1 (~ 0 7 2))
                      (lazy-reshape 2 (~ 1 7 2))))
  (compute (lazy-fuse (lazy-reshape 1 (~ 0 2 ~ 0 2))
                      (lazy-reshape 2 (~ 0 2 ~ 2 4))
                      (lazy-reshape 3 (~ 2 4 ~ 0 2))
                      (lazy-reshape 4 (~ 2 4 ~ 2 4)))))

(document-function lazy-overwrite
  "Returns a lazy array that is a combination of all the values of all the
supplied arrays.  If any of the supplied arrays overlap at some index, the
value of the result in that index is that of the rightmost array containing
that index.  Signals an error unless all the supplied arrays have the same
rank, or if the union of all their shapes cannot be represented as a
shape."
  (compute (lazy-overwrite (lazy-reshape 1 (~ 0 4))
                           (lazy-reshape 2 (~ 2 4))))
  (compute (lazy-overwrite (lazy-reshape 1 (~ 3 ~ 3))
                           (lazy-reshape 2 (~ 1 2 ~ 1 2)))))

(document-function lazy-index-components
  "Returns a lazy array containing the index components of the designated
shape in the supplied axis.  If the first argument is not a shape, the function
SHAPE-DESIGNATOR-SHAPE is used to convert it a shape.  If no axis is not
supplied, it defaults to zero."
  (compute (lazy-index-components (~ 9)))
  (compute (lazy-index-components (~ 10 30 2)))
  (compute (lazy-index-components (~ 4 ~ 4) 0))
  (compute (lazy-index-components (~ 4 ~ 4) 1))
  (compute (lazy-index-components #2a((1 2) (3 4)) 1)))

(document-function lazy-sort
  "Returns a lazy array containing the elements of the supplied array, but sorted
along the first axis according to the supplied predicate and key function.  If
the key function is not supplied, it defaults to the identity function.  For
any two elements, the results of invoking the key function are passed to the
predicate to determine whether the first element is strictly less than the
second one.  As a second value, returns a lazy array of the same shape that
contains the keys corresponding to each of the sorted elements."
  (compute (lazy-sort #(1 3 7 5 0 6 4 9 8 2) #'<))
  (compute (lazy-sort "Sphinx of black quartz, judge my vow." #'char-lessp))
  (multiple-value-call #'compute (lazy-sort #(-2 -1 0 1 2) #'> :key #'abs))
  (compute (lazy-sort #2a((9 8 7) (6 5 4) (3 2 1)) #'<)))

(document-function lazy-stack
  "Returns a lazy array whose contents are the supplied arrays, stacked next
to each other along a particular axis, such that the leftmost array will have
the lowest index components, and the rightmost array will have the highest
index components.  Keyword arguments can be supplied to specify the axis along
which to stack, and the start and step of the resulting lazy array in that
axis."
  (compute (lazy-stack (list 1 2 #(3 4))))
  (compute (lazy-stack (list #2A((1 2) (3 4)) #2A((5 6) (7 8)))))
  (compute (lazy-stack (list #2A((1 2) (3 4)) #2A((5 6) (7 8))) :axis 1)))

(document-function lazy-reduce
  "Returns the lazy arrays that are reduction of the supplied
arrays with the supplied function.  For a single supplied array that is a
vector, this function operates as follows:

1. If the vector has zero elements, return the scalar that is the result of
   invoking the supplied function with zero arguments.  This behavior is
   analogous with Common Lisp's REDUCE function and allows graceful handling of
   many built-in functions.

2. If the vector has an even number of elements, split it into two vectors of
   half the size of the original one, where the first vector contains all the
   elements with an even index, and the second vector contains all the elements
   with an odd index.  Map the supplied function, which must accept two
   arguments and return one value, over these two vectors to obtain a single
   vector of results.  Process the resulting vector with step 2 or step 3,
   depending on whether it has an even or an odd number of elements.

3. If the vector has an odd number of elements distinguish three cases:

   a) If there is a leftover element from one of the previous steps, append it
      at the end of the vector.  The resulting vector has an even number of
      elements.  Process it with step 2.

   b) Otherwise, if the vector has exactly one element, this element is the
      result of the reduction.  Return it.

   c) Otherwise, remove the last element of the vector and store it as the
      leftover element for use in step 3a.  Process the remaining elements with
      step 2.

In addition to this simple case of reducing a vector into a scalar, this
function supports three further generalizations:

- If the argument is an array with rank larger than one, the reduction is
  carried out along the first axis only, and all the remaining axes are handled
  by carrying out multiple reductions in parallel.  In that case, the result is
  not a scalar but an array whose rank is one less than before.

- Instead of supplying a function as the first argument, one may also supply a
  list of functions.  In that case, the supplied arrays are first reduced with
  the first of those functions, the results thereof are reduced with the second
  of those functions, and so on.

- Instead of reducing a single array with a function that takes two arguments
  and returns one value, one may also reduce k arrays with a function that
  takes 2k arguments and returns k values.  In that case, all the supplied
  arrays are first broadcast to have the same shape, and these arrays are
  processed exactly like in the case of reducing a single vector except that
  values are selected from all k arrays simultaneously, resulting in 2k
  arguments being passed to the supplied function, and returning k lazy arrays
  as a result.

A final piece of advice: when reducing a vector that is possibly empty, it is
advisable to stack a neutral element at the beginning or the end of that vector
to make it non-empty.
"
  (compute (lazy-reduce '+ #()))
  (compute (lazy-reduce '+ #(1 2 3 4)))
  (compute (lazy-reduce '+ #2a((1 2) (3 4))))
  (compute (lazy-reduce '(+ +) #2a((1 2) (3 4))))
  (let ((a #(5 2 7 1 9 6)))
    (multiple-value-bind (max index)
        (lazy-reduce
         (lambda (lv li rv ri)
           (if (> lv rv)
               (values lv li)
               (values rv ri)))
         a (lazy-index-components a))
      (compute max index)))
  (compute (lazy-reduce 'min (lazy-stack (list 0 #())))))

(document-function lazy-rearrange
  "Returns a lazy array with the same contents as the supplied one, but whose
first few axes are replaced by those from a different shape. The three
arguments of this function are the lazy array to be rearranged, the number of
axes to rearrange, and the shape to use instead.  Signals an error if the
original shape and the resulting shape differ in size."
  (compute (lazy-rearrange (lazy-index-components (~ 1 10)) 1 (~ 3 ~ 3)))
  (compute (lazy-rearrange #2a((1 2) (3 4)) 2 (~ 4)))
  (compute (lazy-rearrange #2a((1 2) (3 4)) 1 (~ 2 ~ 1))))

(document-function view
  "View the supplied lazy arrays as a graph, using some external program.  Each
graph node corresponds to one lazy array, and is shown with its shape, derived
element type, and possibly some other attributes.  Each graph edge describes
one data flow dependency between one lazy array and another.")

(document-function differentiator
  "Returns, for the supplied sequence of outputs and the supplied sequence of the
corresponding gradients at each output, a function that can be applied to any
lazy array that is a dependency of any of these outputs to obtain the gradient
at that output.  The gradient of a lazy array is another lazy array with the
same shape that describes how much each value differs from its expected value.")

(document-function deflater
  "Returns a function that can be supplied as a modifier to LAZY-RESHAPE to
move each of the designated axes to have a start of zero and a step size of
one.  The supplied argument must either be a non-negative integer that denotes
the number of axes to deflate, or a bit vector with one element per axis that
is one if the corresponding axis should be deflated and zero otherwise."
  (lazy-reshape 5 (~ 3 33 3) (deflater 1))
  (lazy-reshape 5 (~ 1 8 ~ 1 8 ) (deflater #*01)))

(document-function peeler
  "Returns a function that can be supplied as modifier to LAZY-RESHAPE to
turn any lazy array shape into modifiers that select certain interior points of
that shape.  The nature of this function is determined by the supplied amount
specifiers, one for each axis, each of which can either be an unsigned integer,
or a list of up to three unsigned integers.  The behavior of each amount
specifiers, each of which can either be an unsigned integer, or a list of up to
three unsigned integers.  The behavior of each amount specifier is as such:

- A single unsigned integers designates the number of elements that are to be
  peeled off both at the low and the high end of the corresponding range.

- An empty list means that the corresponding range is not modified.

- An list of one unsigned integer is treated as if that integer was supplied
  instead.

- If the amount specifier is a list of two unsigned integers, the first integer
  denotes the number of elements that are to be peeled off at the low end of
  the corresponding range, and the second integer denotes the amount that is to
  be peeled off of the high end of that range.

- If the amount specifier is a list of three unsigned integers, the fist two
  are interpreted as the low and the high amount as before, and the third
  integer is the relative step size within the selection.

The resulting function signals an error if it is applied to a shape whose rank
is less than the number of supplied amount specifiers."
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeler 1)))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeler 1 '(2 0))))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeler 0 '(0 0 2)))))

(document-function slicer
  "Returns a function that can be supplied as a modifier to LAZY-RESHAPE to turn
any lazy array shape into modifiers that select a particular slice of that
shape.  The nature of this function is determined by the supplied slice
specifiers, one for each axis, each of which is one of the following:

- A single unsigned integer N for selecting the element with relative index N
  and dropping that axis from the resulting shape.

- An empty list for keeping the corresponding range as it is.

- A list with one unsigned integer N for selecting the element with relative
  index N and keeping that axis in the resulting shape.

- A list with two unsigned integers B and E for selecting the elements from the
  relative index B up to right below the relative index E.

- A list with three unsigned integers B, E, and S for selecting the elements
  beginning at relative index B, with a relative step size of S, up to right
  below the relative index E.

All indices are interpreted as relative coordinates, so in a range with a start
of five and step size of two a relative index zero would map to the absolute
index five and a relative index of one would map to the absolute index seven.
Signals an error unless the integers B and E are valid relative bounding
indices for the range being worked one, i.e., B must be less than the size of
that range, and E must be larger than or equal to B and less than or equal to
the size of that range."
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (slicer 0)))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (slicer 1 1)))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (slicer '(1 2))))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (slicer '(0 3 2))))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (slicer '() '(0 3 2)))))

(document-function compute
  "Takes any number of arguments that must be lazy array designators and
returns the same number of possibly specialized regular arrays with the
corresponding computed contents.  Whenever a shape of any of the supplied lazy
arrays has as step size other than one, or an offset other than zero, that
array is deflated before being computed, i.e., each axis is shifted to begin
with zero, and divided by the step size.

As a special case, whenever this function would return an array with rank zero,
it instead returns the sole element of that array.  The reason for this
treatment of scalars is that Petalisp treats every object as an array, whereas
a Common Lisp array of rank zero and the object therein are distinct entities.
Of those two distinct representations, the non-array one is usually more useful
for further processing, so this is the one being returned.

All the heavy lifting in Petalisp happens within this function.  The exact
details of how it operates aren't important for an application programmer, but
it is valuable to understand the rough steps that happen under the hood.  The
individual steps of computing some lazy arrays are as follows:

1. Convert each supplied argument to a lazy array.

2. Reshape each lazy array whose so that it has a step size of one and an
   offset of zero.

3. Determine the dependency graph whose roots are the deflated lazy arrays,
   whose interior nodes are calls to lazy map, lazy reshape, or lazy fuse, and
   whose leaves are wrapped regular arrays or the index components of some
   shape designator.

4. Optimize the dependency graph, discard all unused parts, and plan a schedule
   that is fast and has reasonable memory requirements.

5. Execute the schedule on the available hardware.  Make use of all processing
   units, accelerators, or even distributed systems where possible. Gather the
   results in the form of regular arrays.

6. Change the internal representation of all the originally supplied lazy
   arrays so that future calculations involving them directly use the computed
   results.

7. Return the results as multiple values, while replacing any array with rank
   zero with the single element contained in that array.

This function is the workhorse of Petalisp.  A lot of effort went into making
it not only powerful, but also extremely fast.  The overhead of calling it
instead of invoking an already compiled and optimized program is usually just a
few microseconds, so this may be the only evaluation interface that you ever
need."
  (compute (lazy-array #(1 2 3)))
  (compute #(1 2 3))
  (compute 5)
  (compute #0a42)
  (compute #(1 2 3) 5 #0a42))

(document-function petalisp.core:compute-list-of-arrays
  "Returns a list of computed results - one for each element in the list
of supplied arrays.

The computed result of an array or lazy array with rank zero is the one
element contained in this array.  The computed result of any other array or
lazy array is an array with the same rank and dimensions.  The computed
result of any other object is that object itself."
  (petalisp.core:compute-list-of-arrays (list (lazy #'+ 2 #(3 4))))
  (petalisp.core:compute-list-of-arrays (list (lazy-reshape #2a((1 2) (3 4)) (transform i j to j i))))
  (petalisp.core:compute-list-of-arrays (list 2 #0A3 #(4))))

(document-function compute-asynchronously
  "Hints that it would be worthwhile to compute the supplied arrays
asynchronously.  Returns a request object that can be passed to the
functions WAIT and COMPLETEDP.")

(document-function make-unknown
  "Returns a lazy array with the supplied shape and element type and whose
contents are not known and consequently cannot be computed.  It is an error to
compute lazy arrays that depend on an unknown lazy array.  The main purpose of
this function is to construct the arguments to the EVALUATOR function, and to
construct abstract programs that are meant to be analyzed rather than computed."
  (make-unknown :shape (~ 5 ~ 5))
  (make-unknown :element-type 'double-float))

(document-function evaluator
  "For a supplied list of unknowns lazy arrays of length N and list of lazy
arrays of length K, returns a function with K plus N arguments that returns, as
multiple values, the K array values obtained by computing the supplied arrays
after substituting the Ith unknown with the supplied argument in position K
plus I.  The first K arguments of the resulting evaluator function specify
which storage to use for the results.  A value of NIL indicates that the
corresponding result shall be a fresh array.  A value that is an array ensures
that the result is written to that array.  Signals an error if any of the K
plus N arguments of an evaluator function has a different shape or element type
than the corresponding result or unknown.")

(document-function wait
  "Blocks until all the supplied requests of some COMPUTE-ASYNCHRONOUSLY operations
has been completed.")

(document-function completedp
  "Returns whether all the supplied requests of some COMPUTE-ASYNCHRONOUSLY
operations have been completed.")

(document-function harmonized-element-type
  "Returns the specifier for a type to which all elements of the supplied list of
lazy array designators can be coerced safely.  If the element types of all
supplied lazy arrays are number types, the resulting type is obtained by the
standard rules of numeric contagion (Common Lisp Hyperspec 12.1.4.1 and
12.1.4.4).  Otherwise, the resulting type is one that encompasses the union of
the element types of all supplied lazy arrays."
  (harmonized-element-type (list 5 6f0))
  (harmonized-element-type (list 5d0 #C(0 1)))
  (harmonized-element-type (list 'foo "bar" :baz 42)))

(document-function harmonize
  "Lazily coerce each array in the supplied list of arrays to the common
harmonized element type, and return a list of the resulting lazy arrays."
  (apply #'compute (harmonize (list 5 6f0)))
  (apply #'compute (harmonize (list #C(5 2) (lazy-stack (list 1.0 2.0 3.0))))))

(document-function lazy-fuse-and-harmonize
  "Returns the result of first harmonizing all the supplied arrays, and then
passing them to LAZY-FUSE.")

(document-function lazy-overwrite-and-harmonize
  "Returns the result of first harmonizing all the supplied arrays, and then
passing them to LAZY-OVERWRITE.")

(document-function with-lazy-arrays
  "Execute the body in an environment where each of the supplied symbols is
shadowed by the lazy array equivalent of the previous value of that symbol.  An
alternative notation can be used to avoid shadowing the original array: If any
array name is not a symbol but a list of a symbol and a form, the symbol is
bound to the lazy array equivalent of what's produced by that form.  It is good
practice to start each function that expects some of its arguments to be lazy
arrays to start with a suitable use of this macro."
  (let ((a 5) (b #(1 2 3)))
    (with-lazy-arrays (a (c b)) (values a b c))))
