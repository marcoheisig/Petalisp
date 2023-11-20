;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges

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
second argument is used as an exclusive lower bound."
  (range 5)
  (range 5 9)
  (range 5 13 2)
  (range 1 7 -2)
  (range 7 1 -2))

(document-function rangep
  "Returns whether a supplied object is a range."
  (rangep 42)
  (rangep (range 1 3 2)))

(document-function range-emptyp
  "Returns whether a supplied range is empty."
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
Inputs and outputs are separated by the symbol PETALISP:TO.

Each input can either be a symbol or an integer.  In case the input is a
symbol, it is the name under which the value of that input can be referenced in
one of the outputs.  In case the input is a integer, it denotes an input
constraint meaning that it is an error to later apply that transformation to an
index that differs from that constraint in that position.

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
  "Returns whether a supplied object is a transformation."
  (transformationp (transform i j to j i))
  (transformationp (transform i j to i j))
  (transformationp 42))

(document-function petalisp.core:identity-transformation-p
  "Returns whether a supplied object is an identity transformation."
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
  (invert-transformation
   (transform a b to a)))

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
  "Returns the shape that results from applying the supplied transformation to
each index of the supplied shape."
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

- :INPUT-RANK A non-negative integer that is the rank of any valid index or
  shape supplied to this transformation.  Defaults to the length of the
  supplied input mask, or, if no input mask is supplied, to the default
  value of the output rank.  Signals an error if neither the input rank or
  the output rank can be derived in any way.

- :OUTPUT-RANK A non-negative integer that is the rank of any valid index
  or shape supplied to this transformation.  Defaults to the length of the
  supplied scalings, offsets, or output mask, or, if none of these are
  supplied, to the default value of the input rank.  Signals an error if
  neither the input rank or the output rank can be derived in any way.

- :INPUT-MASK A sequence with one element per axis of the transformation's
  input. Each element must either be an integer, in which case only this
  integer may occur in the corresponding axis of the input, or NIL, in
  which case any integer may occur in the corresponding axis.

- :OUTPUT-MASK A sequence with one element per axis of the transformation's
  output.  Each element must either be an integer, in which case this
  integer denotes the axis of the input that is to be scaled, shifted and
  sent to the current position's output, or NIL, in which case only the
  corresponding offset value is sent to the current output.  This way, the
  output mask can encode both permutations of the input, as well as
  insertion and removal of axes.  If this keyword argument is not supplied,
  it defaults to a sequence of consecutive integers as long as the minimum
  of the input rank and the output rank, followed by entries of NIL in case
  the output rank exceeds the input rank.

- :SCALINGS A sequence with one element per axis of the transformation's
  output.  Each element must be a rational number.  Every transformation
  output is the input denoted by the output mask, scaled with its
  corresponding entry in this sequence, and then added to the corresponding
  offset.  In case of an output mask entry of NIL, the corresponding
  scaling is ignored and the offset of that output axis is returned as is.
  If this keyword argument is not supplied, it defaults to a sequence of
  ones.

- :OFFSETS A sequence with one element per axis of the transformation's
  output.  Each element must be a rational number that is added to the
  corresponding output value after scaling has taken place.  If this
  keyword argument is not supplied, it defaults to a sequence of zeros.

Signals an error if some of the sequences supplied as :OUTPUT-MASK,
:SCALINGS, or :OFFSETS differ in length."
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
   rank than expected from the transformation, it is broadcast to that rank
   first.  If the lazy array has higher rank than expected from the
   transformation, those extra axes are left as is, and end up being appended
   to the shape of the resulting lazy array.

2. If the modifier is a function, apply it to the shape of the lazy
   array to obtain a number of new modifiers as multiple values.  Process the
   new modifiers as if they were supplied instead of this function modifier.

3. If the modifier is a shape designator then each axis of the lazy array is
   moved, broadcast, or selected-from to match that shape.  If the lazy array
   has lower rank than the designated shape, it is broadcast to that rank
   first.  If the lazy array has higher rank than the designated shape, the
   remaining axes are left as is, and end up being appended to the shape of the
   resulting lazy array.  For each axis, the mapping from the range of the lazy
   array to the corresponding range of designated shape is derived as such:

   a) If both the source range and the target range have the same size, then
      elements of that axis are moved so that they end up on the target range
      while maintaining the original order.

   b) If the source range has a size of one, then that axis is broadcast to the
      target range.

   c) If the target range is a proper subset of the source range, select only
      those elements of that axis that fall within the target range.

Returns the lazy array obtained after processing all modifiers."
  (compute (lazy-reshape #2A((1 2) (3 4)) (transform i j to j i)))
  (compute (lazy-reshape #(1 2 3 4) (transform i to (- i))))
  (compute (lazy-reshape #(1 2 3 4) (transform i to (- i)) (~ -2 0)))
  (compute (lazy-reshape #(1 2 3 4 5 6) (~ 1 5)))
  (compute (lazy-reshape #(1 2 3 4 5 6) (~ 1 5)))
  (compute (lazy-reshape #(1 2 3 4 5 6) (~ 2 ~ 3)))
  (compute (lazy-reshape #(1 2 3 4 5 6) (~ 6 ~ 2)))
  (compute (lazy-reshape #(1 2 3 4 5 6) (lambda (s) (~ 1 (1- (shape-size s)))))))

(document-function broadcast
  "Returns a list of lazy arrays that all have the same shape, where each lazy array is
a broadcasting reference to the corresponding element of the supplied list of
arrays or scalars.  As a second value, returns the shape of all the resulting
lazy arrays.  Signals an error if there is no suitable shape to which all the
supplied arrays can be broadcast.

The resulting shape is constructed according to the following rules:

1. The rank of the resulting shape is the maximum of the rank of all the
   supplied arrays.

2. Each range of any of the supplied arrays must either have a size of one, in
   which case it is broadcast to the to the corresponding range of the
   resulting shape, or it must be equal to the range of the resulting shape in
   the same axis.

3. The missing axes of Lazy arrays that have a lower rank than the resulting
   shape are broadcast as if they had a range of size one.

4. In case there is an axis in which all supplied arrays have a range with only
   a single element, the resulting shape uses the range of the leftmost
   supplied array."
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
supplied argument, whose contents are the results of applying the function
that is the second supplied argument element-wise to the contents of the
remaining argument arrays.  If the arguments don't agree in shape, they are
first broadcast with the function BROADCAST."
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

(document-function lazy-drop-axes
  "Returns a lazy array with the same contents as the supplied array, but
whose shape has all ranges referred to by the supplied axes removed.  All of
the ranges being referred to must have a size of one."
  (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 3)) 1)
  (compute (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 3)) 1))
  (compute (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 3)) 0 1))
  (compute (lazy-drop-axes (lazy-reshape 1 (~ 1 2 ~ 2 5)) 0)))

(document-function lazy-slice
  "Returns a lazy array whose rank is one less than the rank of the supplied
array, and that contains all entries of the supplied array whose index
component is equal to the supplied index in the optionally supplied axis.
If the axis is not supplied, it defaults to zero."
  (compute (lazy-slice #(1 2 3 4) 2))
  (compute (lazy-slice #2A((1 2) (3 4)) 0))
  (compute (lazy-slice #2A((1 2) (3 4)) 1))
  (compute (lazy-slice #2A((1 2) (3 4)) 0 1))
  (compute (lazy-slice #2A((1 2) (3 4)) 1 1)))

(document-function lazy-slices
  "Returns a lazy array containing all those elements of the supplied array
whose index components in the optionally supplied axis are contained in the
supplied range.  If the axis is not supplied, it defaults to zero.  The
resulting array has the same shape as the supplied array, except that its
range in the axis being sliced along is the supplied range.  Signals an
error if the supplied range is not fully contained in the original range of
that axis."
  (compute (lazy-slices #(1 2 3 4) (range 0 3 2)))
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

(document-function lazy-sort
  "Returns a lazy array containing the elements of the supplied array, but sorted
along the first axis with the supplied predicate."
  (compute (lazy-sort #(1 3 7 5 0 6 4 9 8 2) #'<))
  (compute (lazy-sort "Sphinx of black quartz, judge my vow." #'char-lessp))
  (compute (lazy-sort #2a((9 8 7) (6 5 4) (3 2 1)) #'<)))

(document-function lazy-stack
  "Returns a lazy array whose contents are the supplied arrays, stacked next
to each other along the specified AXIS such that the leftmost array will have
the lowest index components, and the rightmost array will have the highest
index components.

The supplied arrays must all have the same rank, and also the same ranges
in all but the one axis that is being stacked along.  The range of the
resulting lazy array in that axis that is being stacked along has the same
start as the leftmost corresponding argument range that is non-empty, a
size that is the sum of the sizes of all corresponding ranges, and a step
size is that of the leftmost corresponding argument range that has more
than one element, or one if there is no such range.  Signals an error if
multiple arguments have a range with more than one element but differing
step sizes in the axis being stacked along."
  (compute (lazy-stack 0 #(1) #(2) #(3)))
  (compute (lazy-stack 0 #(1 2) #(3 4) #(5 6)))
  (compute (lazy-stack 0 #2A((1 2) (3 4)) #2A((5 6) (7 8))))
  (compute (lazy-stack 1 #2A((1 2) (3 4)) #2A((5 6) (7 8)))))

(document-function lazy-reduce
  "Returns one or more lazy arrays whose contents are the multiple value
reduction with the supplied function, when applied pairwise to the elements of
the first axis of each of the supplied arrays.  If the supplied arrays don't
agree in shape, they are first broadcast with the function BROADCAST.

The supplied function F must accept 2k arguments and return k values, where k
is the number of supplied arrays.  All supplied arrays must have the same shape
S, which is the cartesian product of some ranges, i.e., S = r_1 x ... r_n,
where each range r_k is a set of integers, e.g., {0, 1, ..., m}.  Returns k
arrays of shape s = r_2 x ... x r_n, whose elements are a combination of the
elements along the first axis of each array according to the following rules:

1. If the given arrays are empty, return k empty arrays.

2. If the first axis of each given array contains exactly one element, drop
   that axis and return arrays with the same content, but with shape s.

3. If the first axis of each given array contains more than one element,
   partition the indices of this axis into a lower half l and an upper half
   u.  Then split each given array into a part with shape l x s and a part
   with shape u x s.  Recursively process the lower and the upper halves of
   each array independently to obtain 2k new arrays of shape s.  Finally,
   combine these 2k arrays element-wise with f to obtain k new arrays with
   all values returned by f.  Return these arrays.

The first argument may also be a list of functions, in which case the supplied
lazy arrays are first reduced with the first of those functions, the results
are reduced with the second of those functions, and so on."
  (compute (lazy-reduce '+ #(1 2 3 4)))
  (compute (lazy-reduce '+ #2a((1 2) (3 4))))
  (compute (lazy-reduce '(+ +) #2a((1 2) (3 4))))
  (let ((a #(5 2 7 1 9)))
    (multiple-value-bind (max index)
        (lazy-reduce
         (lambda (lv li rv ri)
           (if (> lv rv)
               (values lv li)
               (values rv ri)))
         a (lazy-index-components a 0))
      (compute max index))))

(document-function differentiator
  "Returns a function that, for each node in a network whose roots are the
supplied OUTPUTS will return the gradient at that node.

GRADIENTS must be a sequence of the same length as OUTPUTS, and whose
elements are either arrays with or symbols that will be used as the name of
such a parameter.")

(document-function collapsing-reshaper
  "Returns a function that can be supplied as a modifier to LAZY-RESHAPE to
turn any lazy array shape into modifiers that collapse that shape, such
that each range therein starts with zero and has a step size of one.")

(document-function peeling-reshaper
  "Returns a function that can be supplied as modifier to LAZY-RESHAPE to
turn any lazy array shape into modifiers that select certain interior
points of that shape.  The nature of this function is determined by the
four keyword arguments :LAYERS, :LOWER-LAYERS, :UPPER-LAYERS, and :STRIDES.
Each of these keyword arguments can be a non-negative integer, in which
case it applies to each axis of the lazy array being reshaped, or it can be
a sequence of non-negative integers, in which case each element of that
sequence applies only to the corresponding axis.

The :LOWER-LAYERS keyword argument describes how many of the lowest
integers in each range are to be peeled off, and the :UPPER-LAYERS keyword
argument describes how many of the highest integers in each range are to be
peeled off.  The :LAYERS keyword argument, which defaults to zero,
describes the default values for both of the lower and upper layers when
they aren't specified explicitly.  The :STRIDES keyword argument, which
defaults to one, denotes a factor for scaling the original step size of
each range, such that a stride of K means selecting only every Kth element.

The resulting function signals an error if an attempt is made to peel more
layers from a lazy array than the size of the range in that axis."
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeling-reshaper :layers 1)))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeling-reshaper :lower-layers 1)))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeling-reshaper :upper-layers 1)))
  (compute (lazy-reshape #2A((1 2 3) (4 5 6) (7 8 9)) (peeling-reshaper :lower-layers '(0 2)))))

(document-function permuting-reshaper
  "Returns a function that can be supplied as a modifier to LAZY-RESHAPE to
turn any lazy array shape into a permuting transformation.  The supplied
arguments must be non-negative integers that denote the ordering of axes
that is established by that transformation."
  (compute (lazy-reshape #2A((1 2) (3 4)) (permuting-reshaper 1 0)))
  (compute (lazy-reshape #3A(((1 2 3) (4 5 6)) ((7 8 9) (10 11 12))) (permuting-reshaper 0 2 1)))
  (compute (lazy-reshape #3A(((1 2 3) (4 5 6)) ((7 8 9) (10 11 12))) (permuting-reshaper 2 0 1))))

(document-function compute
  "The primary interface for evaluating lazy arrays.  It takes any number of
arguments that must be lazy arrays or objects that can be converted to lazy
arrays with the function LAZY-ARRAY, and returns the same number of possibly
specialized regular arrays with the corresponding contents.  A special case is
that whenever an array with rank zero would be returned, it instead returns the
sole element of that array.  The reason for this treatment of scalars is that
Petalisp treats every object as an array, whereas in Common Lisp the rank zero
array containing an object and that object are distinct entities.  Of those two
distinct representations, the non-array one is much more intuitive and useful
in practice, so this is the one being returned.

Whenever a shape of any of the supplied lazy arrays has as step size other than
one, or an offset other than zero, that array is collapsed before being
computed, i.e., each axis is shifted to begin with zero, and divided by the
step size.

All the heavy lifting in Petalisp happens within this function.  The exact
details of how it operates aren't important for application programmers,
but it is valuable to understand the rough steps that happen internally.
The individual steps it performs are:

1. Convert each supplied argument to a lazy array.

2. Reshape each lazy array whose so that it has a step size of one and an
   offset of zero.

3. Determine the dependency graph whose roots are the collapsed lazy arrays,
   whose interior nodes are calls to lazy map, lazy reshape, or lazy fuse, and
   whose leaves are lazy arrays backed by regular arrays or index components of
   some shape.

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
it not only powerful, but also extremely fast.  The overhead of assembling a
graph of lazy arrays and passing it to COMPUTE instead of invoking an already
compiled and optimized imperative program is usually on the order of just a few
microseconds."
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
  "Returns a lazy array with the supplied shape and element type, whose contents
are not known and consequently cannot be computed.  Lazy arrays depending on
such an array also cannot be computed.  The main purpose such unknown lazy
arrays is to construct the arguments to the EVALUATOR function."
  (make-unknown :shape (~ 5 ~ 5))
  (make-unknown :element-type 'double-float))

(document-function evaluator
  "For a supplied list of unknowns of length N and list of LAZY-ARRAYS of length K,
returns a function with K plus N arguments that returns, as multiple values,
the K array values obtained by computing the supplied arrays after substituting
the Ith unknown with the supplied argument in position K plus I.  The first K
arguments of the resulting evaluator function specify which storage to use for
the results.  A value of NIL indicates that the corresponding result shall be a
fresh array.  A value that is an array ensures that the result is written to
that array.

An error is signaled if any of the K plus N arguments of an evaluator function
has a different shape or element type as the corresponding result or unknown.")

(document-function wait
  "Blocks until the requests resulting from some COMPUTE-ASYNCHRONOUSLY operations
has been completed.")

(document-function completedp
  "Returns whether all the requests resulting from some COMPUTE-ASYNCHRONOUSLY
operations have been completed.")

(document-function harmonized-element-type
  "Returns a type to which all elements of all the supplied lazy arrays can
be coerced safely.  If the element types of all supplied lazy arrays are
number types, the resulting type is obtained by the standard rules of
numeric contagion (Common Lisp Hyperspec 12.1.4.1 and 12.1.4.4).  Otherwise, the
resulting type is one that encompasses the union of the element types of
all supplied lazy arrays."
  (harmonized-element-type 5 6f0)
  (harmonized-element-type 5d0 #C(0 1))
  (harmonized-element-type 'foo 'bar 'baz 42))

(document-function lazy-harmonize
  "Lazily coerce each of the supplied arrays to the common harmonized element
type, and return the resulting lazy arrays as multiple values."
  (multiple-value-call #'compute (lazy-harmonize 5 6f0)))

(document-function lazy-harmonize-list-of-arrays
  "Lazily coerce each array in the supplied list of arrays to the common
harmonized element type, and return a list of the resulting lazy arrays."
  (apply #'compute (lazy-harmonize-list-of-arrays (list 5 6f0))))

(document-function lazy-fuse-and-harmonize
  "Returns the result of first harmonizing all the supplied arrays, and then
passing them to LAZY-FUSE.")

(document-function lazy-overwrite-and-harmonize
  "Returns the result of first harmonizing all the supplied arrays, and then
passing them to LAZY-OVERWRITE.")

(document-function with-lazy-arrays
  "Execute the body in an environment where each of the supplied symbols is
shadowed by the lazy array equivalent of the previous value of that symbol.

An alternative notation can be used to avoid shadowing the original array:
If any array name is not a symbol but a list of a symbol and a form, the
symbol is bound to the lazy array equivalent of what's produced by that
form.

It is good practice to start each function that expects some of its
arguments to be lazy arrays to start with a suitable use of this macro."
  (let ((a 5) (b #(1 2 3)))
    (with-lazy-arrays (a (c b)) (values a b c))))
