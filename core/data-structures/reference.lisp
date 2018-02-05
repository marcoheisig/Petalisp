;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/reference
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/immediate)
  (:export
   #:reference
   #:make-reference))

(in-package :petalisp/core/data-structures/reference)

(defclass reference (non-immediate)
  ((%transformation :initarg :transformation :reader transformation))
  (:documentation
   "Let A be a strided array with domain ΩA, let ΩB be a strided array
index space and let T be a transformation from ΩB to ΩA. Then the reference
of A by ΩB and T is a strided array that maps each index tuple k \in ΩB to
A(T(k))."))

(defgeneric make-reference (object space transformation)
  (:documentation
   "Create an instance of a suitable subclass of reference."))

(define-condition reference-error
  (petalisp-user-error)
  ((%data-structure :initarg :data-structure :reader data-structure)
   (%index-space :initarg :index-space :reader index-space)))

(define-condition rerence-to-non-subspace
  (reference-error)
  ())

(define-condition reference-with-transformation-of-invalid-dimension
  (reference-error)
  ())

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

(defmethod reference or ((object data-structure)
                         (space index-space)
                         (transformation identity-transformation))
  "Drop references with no effect."
  (when (index-space-equality (index-space object) space) object))
