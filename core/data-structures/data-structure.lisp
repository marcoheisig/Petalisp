;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/data-structure
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space)
  (:export
   #:data-structure
   #:element-type
   #:inputs
   #:input
   #:refcount
   #:immediate   #:make-immediate #:make-immediate!
   #:application #:make-application
   #:reduction   #:make-reduction
   #:fusion      #:make-fusion
   #:reference   #:make-reference
   #:broadcast
   #:corresponding-immediate
   #:data-structure-equality
   #:shallow-copy
   #:storage
   #:kernels
   #:operator
   #:unary-operator
   #:binary-operator))

(in-package :petalisp/core/data-structures/data-structure)

(defgeneric inputs (data-structure))

(defgeneric refcount (data-structure))

(defgeneric element-type (data-structure))

(defgeneric transformation (data-structure))

(defclass data-structure ()
  ((%element-type :initarg :element-type :reader element-type)
   (%index-space :initarg :index-space :reader index-space)
   (%refcount :initform 0 :accessor refcount))
  (:documentation
   "A data structure of dimension D is a mapping from elements of
INDEX-SPACE to values of type ELEMENT-TYPE.

INPUTS is a list of data structures on which the definition of this data
structure depends on.

REFCOUNT is an implementation detail. For ordinary data structures it
tracks how many times the data structure appears as an input of another
data structure. For immediate data structures, it tracks how many times the
data structure appears as the source of a kernel."))

(defmethod initialize-instance :after ; reference counting
    ((instance data-structure) &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input))) (inputs instance)))

(defclass immediate (data-structure)
  ((%storage :initarg :storage :accessor storage :initform nil)
   (%transformation :initarg :transformation :accessor transformation)
   (%kernels :initarg :kernels :accessor kernels :initform nil))
  (:documentation
   "An immediate is a data structure whose elements can be referenced in
constant time. It has a STORAGE slot that contains its elements in some
unspecified format. TRANSFORMATION maps indices referencing
the immediate to indices referencing STORAGE.

If KERNELS is a non-empty sequence, it denotes the set of kernels that must
be executed before the immediate is fully initialized."))

(defmethod inputs ((immediate immediate)) nil)

(defclass non-immediate (data-structure)
  ((%inputs :initarg :inputs :reader inputs)))

(defclass application (non-immediate)
  ((%operator :initarg :operator :reader operator))
  (:documentation
   "Let F be a referentially transparent Common Lisp function that accepts
n arguments, and let A1...AN be data structures with index space Ω. The the
application of f to A1...AN is a data structure that maps each index k ∈ Ω
to (F (A1 k) ... (AN k))."))

(defclass reduction (non-immediate)
  ((%binary-operator :initarg :binary-operator :reader binary-operator)
   (%unary-operator :initarg :unary-operator :reader unary-operator)
   (%order :initarg :order :reader order :type (member :up :down :arbitrary)))
  (:documentation
   ;; TODO outdated comment, reduce is now inspired by Richard Bird's foldrn function
   "Let F be a referentially transparent Common Lisp function that accepts
two arguments, and let A be a data structure of dimension n, i.e. a mapping
from each element of the cartesian product of the spaces S1, ..., Sn to
some values. Then the reduction of A by F is a data structure of dimension
n-1 that maps each element k of S1 ⨯ ... ⨯ Sn-1 to the pairwise combination
of the elements {a(i) | i ∈ k ⨯ Sn} by F in some ORDER."))

(defclass fusion (non-immediate) ()
  (:documentation
   "Let A1...AN be strided arrays with equal dimension, each mapping from
an index space Ωk to a set of values.  Furthermore, let the sets Ω1...ΩN be
pairwise disjoint, and let Ωf = ∪ Ω1...Ωk be again a valid index
space. Then the fusion of A1...AN is a data structure that maps each index
i ∈ Ωf to the value of i of the unique strided array Ak whose index space
contains i."))

(defclass reference (non-immediate)
  ((%transformation :initarg :transformation :reader transformation))
  (:documentation
   "Let A be a strided array with domain ΩA, let ΩB be a strided array
index space and let T be a transformation from ΩB to ΩA. Then the reference
of A by ΩB and T is a strided array that maps each index tuple k \in ΩB to
A(T(k))."))

(defgeneric make-immediate (data)
  (:documentation
   "Convert object to a Petalisp immediate with the same dimension, element
type and contents. If DATA is already a Petalisp data structure, simply
return it.")
  (:method ((data-structure data-structure)) data-structure)
  (:method ((object t))
    (make-immediate
     (make-array () :initial-element object :element-type (type-of object)))))

(defgeneric make-immediate! (data-structure)
  (:documentation
   "Change the class of DATA-STRUCTURE to a suitable subclass of
immediate.")
  (:method ((immediate immediate)) immediate))

(defgeneric make-application (function first-input all-inputs)
  (:documentation
   "Create an instance of a suitable subclass of application."))

(defgeneric make-reduction (f g a order)
  (:documentation
   "Create an instance of a suitable subclass of reduction."))

(defgeneric make-fusion (first-input all-inputs)
  (:documentation
   "Create an instance of a suitable subclass of fusion."))

(defgeneric make-reference (object space transformation)
  (:documentation
   "Create an instance of a suitable subclass of reference."))

(defgeneric application (function first-input all-inputs)
  (:documentation
   "Return a -- potentially optimized and simplified -- data structure
equivalent to an instance of class APPLICATION.")
  (:method-combination or)
  (:method or (function (first-input data-structure) (all-inputs list))
    (make-application function first-input all-inputs))
  (:method :around (function first-input (all-inputs sequence))
    (assert (eq first-input (elt all-inputs 0)))
    (call-next-method))
  (:method :around (function (first-input data-structure) (all-inputs sequence))
    (assert (identical all-inputs :test #'index-space-equality :key #'index-space))
    (call-next-method)))

(defgeneric reduction (f g a order)
  (:documentation
   "Return a -- potentially optimized and simplified -- data structure
equivalent to an instance of class REDUCTION.")
  (:method-combination or)
  (:method or (f g (a data-structure) order)
    (make-reduction f g a order))
  (:method :around (f g (a data-structure) order)
    (assert (plusp (dimension a)) (a)
            'reduction-of-data-structure-with-dimension-zero
            :input a)
    (call-next-method)))

(defgeneric fusion (first-element all-elements)
  (:documentation
   "Return the fusion of the sequence ALL-ELEMENTS, i.e. a suitable object
that contains the entries of each element from ALL-ELEMENTS. The elements
of ALL-ELEMENTS must not intersect.

FIRST-ELEMENT must be EQ to the first element of ALL-ELEMENTS. Its sole
purpose is to dispatch on it.")
  (:method-combination or)
  (:method or ((first-element data-structure) (all-elements list))
    (make-fusion first-element all-elements))
  (:method :around (first-element all-elements)
    (assert (eq first-element (elt all-elements 0)))
    (if (= 1 (length all-elements))
        first-element
        (call-next-method)))
  (:method :around ((first-element data-structure) all-elements)
    (assert (identical all-elements :test #'= :key #'dimension) (all-elements)
            'fusion-of-elements-of-different-dimension
            :elements all-elements)
    (call-next-method)))

(defgeneric reference (data-structure index-space transformation)
  (:documentation
   "Return a -- potentially optimized and simplified -- data structure
equivalent to an instance of class REFERENCE.")
  (:method-combination or)
  (:method or ((object data-structure)
               (space index-space)
               (transformation transformation))
    (make-reference object space transformation))
  (:method :around ((data-structure data-structure)
                    (index-space index-space)
                    (transformation transformation))
    (let ((relevant-space (funcall transformation index-space))
          (input-space (index-space data-structure)))
      (assert (and (= (dimension relevant-space) (dimension input-space))
                   (subspace? relevant-space input-space))
              (data-structure index-space)
              'reference-to-non-subspace
              :data-structure data-structure
              :index-space index-space))
    (assert (= (dimension index-space) (input-dimension transformation))
            (transformation)
            'reference-with-transformation-of-invalid-dimension
            :data-structure data-structure
            :transformation transformation)
    (call-next-method))
  ;; Combine consecutive references
  (:method or ((reference reference) (index-space index-space) (transformation transformation))
    (reference (input reference)
               index-space
               (composition (transformation reference) transformation))))

(defgeneric broadcast (data-structure index-space)
  (:documentation
   "Return a broadcasting reference to the elements of OBJECT with the
shape of SPACE.")
  (:method :before ((data-structure data-structure) (index-space index-space))
    (assert (<= (dimension data-structure) (dimension index-space))
            (data-structure index-space)
            'broadcast-with-invalid-dimensions
            :data-structure data-structure
            :index-space index-space)))

(defgeneric corresponding-immediate (data-structure)
  (:documentation
   "Return an immediate with the same shape and element type as
DATA-STRUCTURE.")
  (:method ((immediate immediate)) immediate))

(defgeneric data-structure-equality (data-structure-1 data-structure-2)
  (:method-combination and)
  (:documentation
   "Return whether the given data structures are equal.")
  (:method and ((data-structure-1 data-structure)
                (data-structure-2 data-structure))
    (index-space-equality (index-space data-structure-1)
                          (index-space data-structure-2))))

(defgeneric dimension (object)
  (:documentation
   "Return the number of dimensions of OBJECT.")
  (:method ((object t)) 0)
  (:method ((list list)) (length list))
  (:method ((array array)) (array-rank array))
  (:method ((transformation transformation))
    (let ((input-dimension (input-dimension transformation))
          (output-dimension (output-dimension transformation)))
      (assert (= input-dimension output-dimension))
      input-dimension))
  (:method ((data-structure data-structure))
    (dimension (index-space data-structure))))

(defgeneric shallow-copy (instance)
  (:documentation
   "Make a copy of INSTANCE that behaves similarly, but is not EQ to it.")
  (:method ((immediate immediate))
    (make-instance (class-of immediate)
      :element-type (element-type immediate)
      :index-space (index-space immediate)
      :transformation (transformation immediate)
      :kernels (kernels immediate)
      :storage (storage immediate)))
  (:method ((application application))
    (let ((inputs (inputs application)))
      (make-application (operator application) (first inputs) inputs)))
  (:method ((reduction reduction))
    (make-reduction
     (binary-operator reduction)
     (unary-operator reduction)
     (input reduction)
     (order reduction)))
  (:method ((fusion fusion))
    (let ((inputs (inputs fusion)))
      (make-fusion (first inputs) inputs)))
  (:method ((reference reference))
    (make-reference (input reference) (index-space reference) (transformation reference))))

(defun input (object)
  "Return the unique input of OBJECT."
  (destructuring-bind (input) (inputs object) input))

(defmethod reference or ((object data-structure)
                         (space index-space)
                         (transformation identity-transformation))
  "Drop references with no effect."
  (when (index-space-equality (index-space object) space) object))

(defmethod size ((data-structure data-structure))
  (size (index-space data-structure)))

(defmethod print-object ((object data-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (index-space object) stream)))
