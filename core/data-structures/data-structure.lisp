;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/data-structure
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/data-structure-method-combination
   :petalisp/core/data-structures/index-space)
  (:export
   #:data-structure
   #:immediate
   #:non-immediate
   #:application
   #:reduction
   #:fusion
   #:reference

   #:corresponding-immediate
   #:make-immediate
   #:make-immediate!
   #:shallow-copy
   #:make-application
   #:make-reduction
   #:make-fusion
   #:make-reference
   #:element-type
   #:inputs
   #:input
   #:refcount
   #:storage
   #:kernels
   #:order
   #:operator
   #:binary-operator
   #:unary-operator
   #:broadcast
   #:data-structure-equality))

(in-package :petalisp/core/data-structures/data-structure)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions on Data Structures

(defgeneric make-immediate (data)
  (:method-combination make-data-structure))

(defgeneric make-application (function first-input all-inputs)
  (:method-combination make-data-structure))

(defgeneric make-reduction (f g a order)
  (:method-combination make-data-structure))

(defgeneric make-fusion (first-input all-inputs)
  (:method-combination make-data-structure))

(defgeneric make-reference (data-structure index-space transformation)
  (:method-combination make-data-structure))

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

(defgeneric broadcast (data-structure index-space))

(defgeneric shallow-copy (data-structure))

(defgeneric data-structure-equality (data-structure-1 data-structure-2)
  (:method-combination and))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Class DATA-STRUCTURE and its Direct Subclasses
;;;
;;; A data structure of dimension D is a mapping from elements of
;;; INDEX-SPACE to values of type ELEMENT-TYPE.
;;;
;;; REFCOUNT is an implementation detail. For ordinary data structures it
;;; tracks how many times the data structure appears as an input of another
;;; data structure. For immediate data structures, it tracks how many times the
;;; data structure appears as the source of a kernel.

(defclass data-structure ()
  ((%element-type :initarg :element-type :reader element-type)
   (%index-space :initarg :index-space :reader index-space)
   (%refcount :initform 0 :accessor refcount)))

;;; An immediate is a data structure whose elements can be referenced in
;;; constant time. It has a STORAGE slot that contains its elements in some
;;; unspecified format. TRANSFORMATION maps indices referencing
;;; the immediate to indices referencing STORAGE.
;;;
;;; If KERNELS is a non-empty sequence, it denotes the set of kernels that must
;;; be executed before the immediate is fully initialized.
(defclass immediate (data-structure)
  ((%storage :initarg :storage :accessor storage :initform nil)
   (%transformation :initarg :transformation :accessor transformation)
   (%kernels :initarg :kernels :accessor kernels :initform nil)))

(defclass non-immediate (data-structure)
  ((%inputs :initarg :inputs :reader inputs)))

;;; Let F be a referentially transparent Common Lisp function that accepts
;;; n arguments, and let A1...AN be data structures with index space Ω. The
;;; the application of f to A1...AN is a data structure that maps each
;;; index k ∈ Ω to (F (A1 k) ... (AN k)).
(defclass application (non-immediate)
  ((%operator :initarg :operator :reader operator)))

;;; The reduction of a D-dimensional array A is a D-1 dimensional array,
;;; where each element contains the result of reducing the last dimension
;;; with BINARY-OPERATOR, using the result of UNARY-OPERATOR applied to the
;;; first element as initial value.
(defclass reduction (non-immediate)
  ((%binary-operator :initarg :binary-operator :reader binary-operator)
   (%unary-operator :initarg :unary-operator :reader unary-operator)
   (%order :initarg :order :reader order :type (member :up :down :arbitrary))))

;;; Let A1...AN be strided arrays with equal dimension, each mapping from
;;; an index space Ωk to a set of values.  Furthermore, let the sets
;;; Ω1...ΩN be pairwise disjoint, and let Ωf = ∪ Ω1...Ωk be again a valid
;;; index space. Then the fusion of A1...AN is a data structure that maps
;;; each index i ∈ Ωf to the value of i of the unique strided array Ak
;;; whose index space contains i.
(defclass fusion (non-immediate) ())

;;; Let A be a strided array with domain ΩA, let ΩB be a strided array
;;; index space and let T be a transformation from ΩB to ΩA. Then the
;;; reference of A by ΩB and T is a strided array that maps each index
;;; tuple k \in ΩB to A(T(k)).
(defclass reference (non-immediate)
  ((%transformation :initarg :transformation :reader transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on DATA-STRUCTUREs

;;; Increase the REFCOUNT of each input of each data structure.
(defmethod initialize-instance :after
    ((instance data-structure) &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input))) (inputs instance)))

;;; Return a broadcasting reference with the given INDEX-SPACE to the
;;; elements of DATA-STRUCTURE.
(defmethod broadcast :before ((data-structure data-structure) (index-space index-space))
  (demand (<= (dimension data-structure) (dimension index-space))
    "~@<Invalid broadcasting reference with space ~A to ~
        a data structure with space ~A.~:@>"
    index-space (index-space data-structure)))

(defmethod data-structure-equality and ((data-structure-1 data-structure)
                                        (data-structure-2 data-structure))
  (index-space-equality (index-space data-structure-1)
                        (index-space data-structure-2)))

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
  (dimension (index-space data-structure)))

(defun input (object)
  (destructuring-bind (input) (inputs object) input))

(defmethod size ((data-structure data-structure))
  (size (index-space data-structure)))

(defmethod print-object ((object data-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (element-type object) (index-space object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on IMMEDIATEs

(defmethod inputs ((immediate immediate))
  nil)

(defmethod make-immediate :optimize ((immediate immediate))
  immediate)

(defmethod make-immediate ((data-structure data-structure))
  ;; TODO doesn't make sense. The protocol needs to be redesigned...
  data-structure)

(defmethod make-immediate ((object t))
  (make-immediate
   (make-array () :initial-element object :element-type (type-of object))))

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
  (assert (identical all-inputs :test #'index-space-equality :key #'index-space)))

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

(defmethod make-fusion :check (first-index-space (index-spaces sequence))
  (assert (eq first-index-space (elt index-spaces 0)))
  (unless (= 1 (length index-spaces))
    (map-combinations
     (lambda-match
       ((list a b)
        (demand (= (dimension a) (dimension b))
          "~@<The index spaces of the arguments to a fusion operation ~
              must have the same dimension, but the supplied arguments are ~
              ~R- and ~R-dimensional data structures.~:@>"
          (dimension a)
          (dimension b))
        (let ((space-1 (index-space a))
              (space-2 (index-space b)))
          (demand (not (index-space-intersection-p space-1 space-2))
            "~@<The index spaces of the arguments to a fusion operation ~
                must be disjoint, but space ~S and space ~S have the ~
                common subspace ~S.~:@>"
            space-1
            space-2
            (index-space-intersection space-1 space-2)))))
     index-spaces
     :length 2
     :copy nil)))

;;; ignore one-index-space fusions
(defmethod make-fusion :optimize (first-index-space (index-spaces sequence))
  (when (= 1 (length index-spaces))
    first-index-space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized on REFERENCEs

(defmethod make-reference :check
    ((data-structure data-structure)
     (index-space index-space)
     (transformation transformation))
  (let ((relevant-space (funcall transformation index-space))
        (input-space (index-space data-structure)))
    (demand (and (= (dimension relevant-space) (dimension input-space))
                 (subspace-p relevant-space input-space))
      "~@<The index space referenced by the current reference is ~S, ~
          which is not a subspace of ~S, the index space of the input of ~
          the current reference.~:@>"
      relevant-space
      input-space))
  (demand (= (dimension index-space) (input-dimension transformation))
    "~@<The dimension of the index space of a reference operation must ~
        be equal to the input dimension of its transformation. The ~
        index space ~S has the dimension ~R, but the input dimension ~
        of the transformation ~S is ~R.~:@>"
    index-space
    (dimension index-space)
    transformation
    (input-dimension transformation)))

;;; Combine consecutive references
(defmethod make-reference :optimize
    ((reference reference)
     (index-space index-space)
     (transformation transformation))
  (make-reference
   (input reference)
   index-space
   (compose-transformations
    (transformation reference)
    transformation)))

;;; Drop references with no effect.
(defmethod make-reference :optimize
    ((data-structure data-structure)
     (index-space index-space)
     (identity-transformation identity-transformation))
  (when (index-space-equality (index-space data-structure) index-space)
    data-structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating shallow copies of data structures
;;;
;;; If Petalisp were entirely functional, there would be no need for such
;;; devious functions as SHALLOW-COPY and MAKE-IMMEDIATE!. However, in
;;; order to avoid endless reevaluation, the data flow graphs that define
;;; each data structure must be cut from time to time. The current policy
;;; is to cut the graph at all nodes that are passed to SCHEDULE and
;;; COMPUTE. Ugh!

(defmethod make-immediate! ((immediate immediate))
  immediate)

(defmethod shallow-copy ((immediate immediate))
  (make-instance (class-of immediate)
    :element-type (element-type immediate)
    :index-space (index-space immediate)
    :transformation (transformation immediate)
    :kernels (kernels immediate)
    :storage (storage immediate)))

(defmethod shallow-copy ((application application))
  (let ((inputs (inputs application)))
    (make-application (operator application) (first inputs) inputs)))

(defmethod shallow-copy ((reduction reduction))
  (make-reduction
   (binary-operator reduction)
   (unary-operator reduction)
   (input reduction)
   (order reduction)))

(defmethod shallow-copy ((fusion fusion))
  (let ((inputs (inputs fusion)))
    (make-fusion (first inputs) inputs)))

(defmethod shallow-copy ((reference reference))
  (make-reference (input reference)
                  (index-space reference)
                  (transformation reference)))
