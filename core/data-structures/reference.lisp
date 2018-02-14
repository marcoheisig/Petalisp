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

;;; Let A be a strided array with domain ΩA, let ΩB be a strided array
;;; index space and let T be a transformation from ΩB to ΩA. Then the
;;; reference of A by ΩB and T is a strided array that maps each index
;;; tuple k \in ΩB to A(T(k)).
(defclass reference (non-immediate)
  ((%transformation :initarg :transformation :reader transformation)))

(defgeneric make-reference (object space transformation))

(defgeneric reference (data-structure index-space transformation)
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
      (input-dimension transformation))
    (call-next-method))
  ;; Combine consecutive references
  (:method or ((reference reference) (index-space index-space) (transformation transformation))
    (reference (input reference)
               index-space
               (composition (transformation reference) transformation))))

;;; Drop references with no effect.
(defmethod reference or ((object data-structure)
                         (space index-space)
                         (transformation identity-transformation))
  (when (index-space-equality (index-space object) space)
    object))
