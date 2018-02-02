;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/immediate
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure)
  (:export
   #:immediate
   #:non-immediate
   #:make-immediate
   #:make-immediate!
   #:corresponding-immediate))

(in-package :petalisp/core/data-structures/immediate)

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

(defclass non-immediate (data-structure)
  ((%inputs :initarg :inputs :reader inputs)))

(defmethod inputs ((immediate immediate)) nil)

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

(defgeneric corresponding-immediate (data-structure)
  (:documentation
   "Return an immediate with the same shape and element type as
DATA-STRUCTURE.")
  (:method ((immediate immediate)) immediate))
