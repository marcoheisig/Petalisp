;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/data-structure
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space)
  (:export
   #:data-structure
   #:element-type
   #:inputs
   #:input
   #:refcount
   #:broadcast
   #:data-structure-equality
   #:storage
   #:kernels
   #:operator
   #:unary-operator
   #:binary-operator))

(in-package :petalisp/core/data-structures/data-structure)

(defgeneric inputs (data-structure))

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

(define-condition broadcast-with-invalid-dimensions
  (petalisp-user-error)
  ((%data-structure :initarg :data-structure :reader data-structure)
   (%index-space :initarg :index-space :reader index-space)))

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

(defun input (object)
  "Return the unique input of OBJECT."
  (destructuring-bind (input) (inputs object) input))

(defmethod size ((data-structure data-structure))
  (size (index-space data-structure)))

(defmethod print-object ((object data-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (index-space object) stream)))
