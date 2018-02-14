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

;;; INPUTS is a list of data structures on which the definition of this
;;; data structure depends on.
(defgeneric inputs (data-structure))


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

;;; Increase the REFCOUNT of each input of each data structure.
(defmethod initialize-instance :after
    ((instance data-structure) &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input))) (inputs instance)))

;;; Return a broadcasting reference with the given INDEX-SPACE to the
;;; elements of DATA-STRUCTURE.
(defgeneric broadcast (data-structure index-space)
  (:method :before ((data-structure data-structure) (index-space index-space))
    (demand (<= (dimension data-structure) (dimension index-space))
      "~@<Invalid broadcasting reference with space ~A to ~
          a data structure with space ~A.~:@>"
      index-space (index-space data-structure))))

(defgeneric data-structure-equality (data-structure-1 data-structure-2)
  (:method-combination and)
  (:method and ((data-structure-1 data-structure)
                (data-structure-2 data-structure))
    (index-space-equality (index-space data-structure-1)
                          (index-space data-structure-2))))

(defgeneric dimension (object)
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
  (destructuring-bind (input) (inputs object) input))

(defmethod size ((data-structure data-structure))
  (size (index-space data-structure)))

(defmethod print-object ((object data-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (element-type object) (index-space object))))
