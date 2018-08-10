;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions on Data Structures

(defgeneric canonicalize-data-structure (data)
  (:method-combination data-structure-constructor))

(defgeneric make-application (function first-input all-inputs)
  (:method-combination data-structure-constructor))

(defgeneric make-reduction (f g a order)
  (:method-combination data-structure-constructor))

(defgeneric make-fusion (first-input all-inputs)
  (:method-combination data-structure-constructor))

(defgeneric make-reference (data-structure shape transformation)
  (:method-combination data-structure-constructor))

(defgeneric corresponding-immediate (data-structure))

(defgeneric make-immediate! (data))

(defgeneric element-type (data-structure))

(defgeneric inputs (data-structure))

(defgeneric refcount (data-structure))

(defgeneric storage (data-structure))

(defgeneric kernels (data-structure))

(defgeneric order (reduction))

(defgeneric operator (data-structure))

(defgeneric binary-operator (data-structure))

(defgeneric unary-operator (data-structure))

(defgeneric broadcast (data-structure shape))

(defgeneric shallow-copy (data-structure))

(defgeneric data-structure-equality (a b)
  (:method-combination and))

(defgeneric size (object)
  (:method ((any-set any-set))
    (set-size any-set))
  (:method ((hash-table hash-table))
    (hash-table-count hash-table))
  (:method ((array array))
    (array-total-size array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Class DATA-STRUCTURE and its Direct Subclasses
;;;
;;; A data structure of dimension D is a mapping from elements of
;;; SHAPE to values of type ELEMENT-TYPE.
;;;
;;; REFCOUNT is an implementation detail. For ordinary data structures it
;;; tracks how many times the data structure appears as an input of another
;;; data structure. For immediate data structures, it tracks how many times the
;;; data structure appears as the source of a kernel.

(defclass data-structure ()
  ((%element-type :initarg :element-type :reader element-type)
   (%shape :initarg :shape :reader shape :reader shape)
   (%refcount :initform 0 :accessor refcount)))

;;; An immediate is a data structure whose elements can be referenced in
;;; constant time. It has a STORAGE slot that contains its elements in some
;;; unspecified format. TRANSFORMATION maps indices referencing
;;; the immediate to indices referencing STORAGE.
;;;
;;; If KERNELS is a non-empty sequence, it denotes the set of kernels that must
;;; be executed before the immediate is fully initialized.
(defclass immediate (data-structure)
  ((%storage :initarg :storage :accessor storage :initform nil :accessor storage-array)
   (%kernels :initarg :kernels :accessor kernels :initform nil)))

(defclass non-immediate (data-structure)
  ((%inputs :initarg :inputs :reader inputs)))

;;; Let F be a referentially transparent Common Lisp function that accepts
;;; n arguments, and let A1...AN be data structures with index shape Ω. The
;;; the application of f to A1...AN is a data structure that maps each
;;; index k ∈ Ω to (F (A1 k) ... (AN k)).
(defclass application (non-immediate)
  ((%operator :initarg :operator :reader operator :reader application-operator)))

;;; The reduction of a D-dimensional array A is a D-1 dimensional array,
;;; where each element contains the result of reducing the last dimension
;;; with BINARY-OPERATOR, using the result of UNARY-OPERATOR applied to the
;;; first element as initial value.
(defclass reduction (non-immediate)
  ((%binary-operator :initarg :binary-operator :reader binary-operator :reader reduction-binary-operator)
   (%unary-operator :initarg :unary-operator :reader unary-operator :reader reduction-unary-operator)
   (%order :initarg :order :reader order :type (member :up :down :arbitrary) :reader reduction-order)))

;;; Let A1...AN be strided arrays with equal dimension, each mapping from
;;; an index shape Ωk to a set of values.  Furthermore, let the sets
;;; Ω1...ΩN be pairwise disjoint, and let Ωf = ∪ Ω1...Ωk be again a valid
;;; index shape. Then the fusion of A1...AN is a data structure that maps
;;; each index i ∈ Ωf to the value of i of the unique strided array Ak
;;; whose index shape contains i.
(defclass fusion (non-immediate) ())

;;; Let A be a strided array with domain ΩA, let ΩB be a strided array
;;; index shape and let T be a transformation from ΩB to ΩA. Then the
;;; reference of A by ΩB and T is a strided array that maps each index
;;; tuple k \in ΩB to A(T(k)).
(defclass reference (non-immediate)
  ((%transformation :initarg :transformation :reader transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on DATA-STRUCTUREs

(defmethod size ((data-structure data-structure))
  (set-size (shape data-structure)))

;;; Increase the REFCOUNT of each input of each data structure.
(defmethod initialize-instance :after
    ((instance data-structure) &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input))) (inputs instance)))

;;; Return a broadcasting reference with the given SHAPE to the
;;; elements of DATA-STRUCTURE.
(defmethod broadcast :before ((data-structure data-structure) (shape shape))
  (demand (<= (dimension data-structure) (dimension shape))
    "~@<Invalid broadcasting reference with shape ~A to ~
        a data structure with shape ~A.~:@>"
    shape (shape data-structure)))

(defmethod data-structure-equality and ((data-structure-1 data-structure)
                                        (data-structure-2 data-structure))
  (set-equal (shape data-structure-1)
                (shape data-structure-2)))

(defmethod canonicalize-data-structure ((data-structure data-structure))
  data-structure)

(defmethod dimension ((object t))
  0)

(defmethod dimension ((list list))
  (length list))

(defmethod dimension ((array array))
  (array-rank array))

(defmethod dimension ((transformation transformation))
  (let ((input-dimension (input-dimension transformation))
        (output-dimension (output-dimension transformation)))
    (assert (= input-dimension output-dimension))
    input-dimension))

(defmethod dimension ((data-structure data-structure))
  (dimension (shape data-structure)))

(defun input (object)
  (destructuring-bind (input) (inputs object) input))

(defmethod print-object ((object data-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (element-type object) (shape object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on IMMEDIATEs

(defmethod inputs ((immediate immediate))
  nil)

(defmethod corresponding-immediate ((immediate immediate))
  immediate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on APPLICATIONs

(defmethod make-application :check
    (function first-input (all-inputs sequence))
  (declare (ignore function))
  (assert (eq first-input (elt all-inputs 0))))

(defmethod make-application :check
    ((function function)
     (first-input data-structure)
     (all-inputs sequence))
  (assert (identical all-inputs :test #'set-equal
                                :key #'shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on REDUCTIONs

(defmethod make-reduction :check (f g (a data-structure) order)
  (declare (ignore f g order))
  (demand (plusp (dimension a))
    "~@<Can only reduce data structures with dimension greater than zero.~:@>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on FUSIONs

(defmethod make-fusion :check (first-shape (shapes sequence))
  (assert (eq first-shape (elt shapes 0)))
  (unless (= 1 (length shapes))
    (map-combinations
     (trivia:lambda-match
       ((list a b)
        (demand (= (dimension a) (dimension b))
          "~@<The index shapes of the arguments to a fusion operation ~
              must have the same dimension, but the supplied arguments are ~
              ~R- and ~R-dimensional data structures.~:@>"
          (dimension a)
          (dimension b))
        (let ((shape-1 (shape a))
              (shape-2 (shape b)))
          (demand (not (set-intersectionp shape-1 shape-2))
            "~@<The index shapes of the arguments to a fusion operation ~
                must be disjoint, but shape ~S and shape ~S have the ~
                common subshape ~S.~:@>"
            shape-1
            shape-2
            (set-intersection shape-1 shape-2)))))
     shapes
     :length 2
     :copy nil)))

;;; ignore one-shape fusions
(defmethod make-fusion :optimize (first-shape (shapes sequence))
  (when (= 1 (length shapes))
    first-shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on REFERENCEs

(defmethod make-reference :check
    ((data-structure data-structure)
     (shape shape)
     (transformation transformation))
  (let ((relevant-shape (transform shape transformation))
        (input-shape (shape data-structure)))
    (demand (and (= (dimension relevant-shape) (dimension input-shape))
                 (set-subsetp relevant-shape input-shape))
      "~@<The index shape referenced by the current reference is ~S, ~
          which is not a subshape of ~S, the index shape of the input of ~
          the current reference.~:@>"
      relevant-shape
      input-shape))
  (demand (= (dimension shape) (input-dimension transformation))
    "~@<The dimension of the index shape of a reference operation must ~
        be equal to the input dimension of its transformation. The ~
        index shape ~S has the dimension ~R, but the input dimension ~
        of the transformation ~S is ~R.~:@>"
    shape
    (dimension shape)
    transformation
    (input-dimension transformation)))

;;; Combine consecutive references
(defmethod make-reference :optimize
    ((reference reference)
     (shape shape)
     (transformation transformation))
  (make-reference
   (input reference)
   shape
   (compose-transformations
    (transformation reference)
    transformation)))

;;; Drop references with no effect.
(defmethod make-reference :optimize
    ((data-structure data-structure)
     (shape shape)
     (identity-transformation identity-transformation))
  (when (set-equal (shape data-structure) shape)
    data-structure))

(defmethod shape ((array array))
  (make-shape (array-dimensions array)))

(defmethod transform ((data-structure data-structure) (operator identity-transformation))
  data-structure)
