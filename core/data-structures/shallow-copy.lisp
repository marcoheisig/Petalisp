;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/shallow-copy
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure)
  (:export
   #:shallow-copy))

(in-package :petalisp/core/data-structures/shallow-copy)

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
