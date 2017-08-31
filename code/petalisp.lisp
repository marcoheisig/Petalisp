;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp Vocabulary - Classes

(in-package :petalisp)

(define-class transformation (unary-funcallable-object) ()
  (:metaclass funcallable-standard-class))

(define-class data-structure ()
  ((element-type :initform t)
   (predecessors :initform nil
                 :type list)))

(define-class index-space (data-structure)
  ())

(define-class application (data-structure)
  ((operator :type function)))

(define-class reduction (data-structure)
  ((operator :type function)))

(define-class repetition (data-structure)
  ())

(define-class fusion (data-structure)
  ())

(define-class reference (data-structure)
  ((transformation :type transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp Vocabulary - Generic Functions

(defgeneric application (f a1 &rest a2...aN)
  (:documentation
   "Let F be a referentially transparent Common Lisp function that accepts
   n arguments, and let A1...AN be data structures with index space Ω. The
   the application of f to A1...AN is a data structure that maps each index
   k ∈ Ω to (F (A1 k) ... (AN k)).")
  (:method :before ((f function) (a1 data-structure) &rest a2...aN)
    (assert (every #'data-structure? a2...aN))
    (let/de ((a1...aN (list* a1 a2...aN)))
      (check-arity f (length a1...aN))
      (assert (identical a1...aN :test #'equal? :key #'index-space)))))

(defgeneric binary-product (object-1 object-2)
  (:method ((a number) (b number))
    (* a b)))

(defgeneric binary-sum (object-1 object-2)
  (:method ((a number) (b number))
    (+ a b)))

(defgeneric classify-transformation (function input-constraints nargout)
  (:documentation
   "Convert an ordinary Common Lisp function FUNCTION to an appropriate
   instance of the class TRANSFORMATION.")
  (:method ((function transformation) input-constraints nargout) function))

(defgeneric composition (g f)
  (:documentation
   "Returns a funcallable object such that its application is equivalent to
   the application of f, followed by an application of g.")
  (:method ((g function) (f function))
    (alexandria:compose g f))
  (:method :before ((g transformation) (f transformation))
    (assert (= (input-dimension g) (output-dimension f)))))

(defgeneric depetalispify (object)
  (:documentation
   "If OBJECT is a Petalisp data structure, return an array with the
   dimension, element type and contents of OBJECT. Otherwise return
   OBJECT.")
  (:method ((object t)) object))

(defgeneric difference (space-1 space-2)
  (:documentation
   "Return a list of index spaces that denote exactly those indices of
   SPACE-1 that are not indices of SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric dimension (object)
  (:documentation
   "Return the number of dimensions of OBJECT.")
  (:method ((object t)) 0)
  (:method ((object array)) (length (array-dimensions object))))

(defgeneric enqueue (object)
  (:documentation
   "Instruct Petalisp to compute the elements of OBJECT
   asynchronously. Return NIL."))

(defgeneric equal? (a b)
  (:documentation
   "Two objects are EQUAL? if their use in Petalisp will always result in
  identical behavior.")
  (:method ((a t) (b t)) (eql a b))
  (:method ((a structure-object) (b structure-object)) (equalp a b)))

(defgeneric fusion (a1 &rest a2...aN)
  (:documentation
   " Let A1...AN be strided arrays with equal dimension, each mapping from
  an index space Ωk to a set of values.  Furthermore, let the sets Ω1...ΩN
  be pairwise disjoint, and let Ωf = ∪ Ω1...Ωk be again a valid index
  space. Then the fusion of A1...AN is a data structure that maps each
  index i ∈ Ωf to the value of i of the unique strided array Ak whose index
  space contains i.")
  (:method :before ((a1 data-structure) &rest a2...aN)
    (let/de ((a1...aN (list* a1 a2...aN)))
      (assert (identical a1...aN :test #'= :key #'dimension)))))

(defgeneric index-space (object)
  (:documentation
   "Return the INDEX-SPACE of OBJECT, i.e. a data structure whose elements
are the indices of OBJECT.")
  (:method ((object index-space))
    object))

(defgeneric input-dimension (transformation)
  (:documentation
   "Return the number of dimensions that a data structure must have to be a
   valid argument for TRANSFORMATION.")
  (:method ((A matrix)) (matrix-n A)))

(defgeneric intersection (space-1 space-2)
  (:documentation
   "Return an index space containing all indices that occur both in SPACE-1
   and SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric inverse (transformation)
  (:documentation
   "Return a transformation whose composition with the argument of this
function is the identity transformation."))

(defgeneric output-dimension (transformation)
  (:documentation
   "Return the number of dimensions of data structures generated by
   TRANSFORMATION.")
  (:method ((A matrix)) (matrix-m A)))

(defgeneric petalispify (object)
  (:documentation
   "If OBJECT is a Lisp array, convert it to a Petalisp data structure with
   the same dimension, element type and contents. If OBJECT is already a
   Petalisp data structure, return OBJECT. Otherwise return a
   zero-dimensional Petalisp data structure with OBJECT as its sole
   element.")
  (:method ((object data-structure)) object)
  (:method ((object t))
    (make-array '() :initial-element object
                    :element-type (type-of object))))

(defgeneric reduction (f a)
  (:documentation
   "Let F be a referentially transparent Common Lisp function that accepts
   two arguments, and let A be a data structure of dimension n, i.e. a
   mapping from each element of the cartesian product of the ranges R1,
   ..., Rn to some values. Then the reduction of A by F is a data structure
   of dimension n-1 that maps each element k of R1 ⨯ ... ⨯ Rn-1 to the
   combination of the elements {a(i) | i ∈ k ⨯ Rn} by F in some arbitrary
   order.")
  (:method :before ((f function) (a data-structure))
    (assert (< 0 (dimension a)))
    (check-arity f 2)))

(defgeneric reference (object space transformation)
  (:method :before ((object data-structure) space (transformation transformation))
    (assert (and (subspace? space object)
                 (= (dimension space) (input-dimension transformation))))))

(defgeneric repetition (object space)
  (:method :before ((object data-structure) space)
    (assert (<= (dimension object) (dimension space)))))

(defgeneric result-type (function &rest arguments))

(defgeneric size (object)
  (:method ((object t)) 1)
  (:method ((object array)) (array-total-size object)))

(defgeneric subspace? (space-1 space-2)
  (:documentation
   "Return true if every index in SPACE-1 occurs also in SPACE-2.")
  (:method ((space-1 t) (space-2 t))
    (equal? space-1 (intersection space-1 space-2))))

(defgeneric wait-for-completion (&rest objects)
  (:documentation
   "Return the computed value of OBJECTS."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp Vocabulary - Non-generic Functions

(declaim (inline sum product))

(defun sum (object &rest more-objects)
  "Returns the sum of the given objects, as computed by BINARY-SUM."
  (if (null more-objects)
      object
      (reduce #'binary-sum more-objects :initial-value object)))

(defun product (object &rest more-objects)
  "Returns the product of the given objects, as computed by BINARY-PRODUCT."
  (if (null more-objects)
      object
      (reduce #'binary-product more-objects :initial-value object)))

(defun predecessor (object)
  "Return the unique predecessor of OBJECT."
  (ematch (predecessors object)
    ((list first) first)))

(defun subdivision (object &rest more-objects)
  "Return a list of disjoint objects. Each resulting object is a proper
subspace of one or more of the arguments and their fusion covers all
arguments."
  (flet ((shatter (dust object)
           (let ((object-w/o-dust (list object)))
             (nconc
              (loop for particle in dust do
                (setf object-w/o-dust
                      (loop for x in object-w/o-dust
                            append (difference x particle)))
                    append (difference particle object)
                    when (intersection particle object) collect it)
              object-w/o-dust))))
    (reduce #'shatter more-objects :initial-value (list object))))
