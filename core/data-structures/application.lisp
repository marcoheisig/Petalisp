;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/application
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/immediate)
  (:export
   #:application
   #:make-application))

(in-package :petalisp/core/data-structures/application)

(defclass application (non-immediate)
  ((%operator :initarg :operator :reader operator))
  (:documentation
   "Let F be a referentially transparent Common Lisp function that accepts
n arguments, and let A1...AN be data structures with index space Ω. The the
application of f to A1...AN is a data structure that maps each index k ∈ Ω
to (F (A1 k) ... (AN k))."))

(defgeneric make-application (function first-input all-inputs)
  (:documentation
   "Create an instance of a suitable subclass of application."))

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
