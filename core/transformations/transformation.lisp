;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/transformation
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all)
  (:export
   #:transformation
   #:input-dimension
   #:output-dimension
   #:composition
   #:inverse
   #:do-outputs
   #:enlarge-transformation))

(in-package :petalisp/core/transformations/transformation)

(define-class transformation (unary-funcallable-object) ()
  (:metaclass funcallable-standard-class)
  (:documentation
   "A transformation is an analytically tractable function from indices to
indices."))

(defgeneric input-dimension (transformation)
  (:documentation
   "Return the dimension that an index space must have to be a valid
argument for TRANSFORMATION.")
  (:method ((A matrix)) (matrix-n A)))

(defgeneric output-dimension (transformation)
  (:documentation
   "Return the dimension of index spaces returned by TRANSFORMATION.")
  (:method ((A matrix)) (matrix-m A)))

(defgeneric composition (g f)
  (:documentation
   "Return g ∘ f, i.e. return a function whose application to some
arguments is equivalent to the application of g to the result of the
application of f to these arguments.")
  (:method ((g function) (f function))
    (alexandria:compose g f))
  (:method :before ((g transformation) (f transformation))
    (assert (= (input-dimension g) (output-dimension f)))))

(defgeneric inverse (transformation)
  (:documentation
   "Return a transformation whose composition with the argument of this
function is the identity transformation."))

(defgeneric do-outputs
    (transformation function &rest input-sequences)
  (:documentation
   "For each output of TRANSFORMATION, invoke FUNCTION with the output
index, input index, the scaling, the offset and the corresponding values of
each sequence in INPUT-SEQUENCES.

Important: When the scaling is zero, the values of the corresponding input
values are undefined."))

(defgeneric enlarge-transformation (transformation scale offset)
  (:documentation
   "Given a transformation mapping from (i1 ... iN) to (j1 ... jM),
return a transformation mapping from (i1 ... iN iN+1) to
(j1 ... jM iN+1)."))
