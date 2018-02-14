;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/reduction
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/immediate)
  (:export
   #:order
   #:reduction
   #:make-reduction))

(in-package :petalisp/core/data-structures/reduction)

(defclass reduction (non-immediate)
  ((%binary-operator :initarg :binary-operator :reader binary-operator)
   (%unary-operator :initarg :unary-operator :reader unary-operator)
   (%order :initarg :order :reader order :type (member :up :down :arbitrary))))

(defgeneric make-reduction (f g a order))

(defgeneric reduction (f g a order)
  (:method-combination or)
  (:method or (f g (a data-structure) order)
    (make-reduction f g a order))
  (:method :around (f g (a data-structure) order)
    (demand (plusp (dimension a))
      "~@<Can only reduce data structures with dimension greater than zero.~:@>")
    (call-next-method)))
