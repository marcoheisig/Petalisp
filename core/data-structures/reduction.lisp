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
   (%order :initarg :order :reader order :type (member :up :down :arbitrary)))
  (:documentation
   ;; TODO outdated comment, reduce is now inspired by Richard Bird's foldrn function
   "Let F be a referentially transparent Common Lisp function that accepts
two arguments, and let A be a data structure of dimension n, i.e. a mapping
from each element of the cartesian product of the spaces S1, ..., Sn to
some values. Then the reduction of A by F is a data structure of dimension
n-1 that maps each element k of S1 ⨯ ... ⨯ Sn-1 to the pairwise combination
of the elements {a(i) | i ∈ k ⨯ Sn} by F in some ORDER."))

(defgeneric make-reduction (f g a order)
  (:documentation
   "Create an instance of a suitable subclass of reduction."))

(define-condition reduction-of-data-structure-with-dimension-zero
    (petalisp-user-error)
    ((%data-structure :initarg :data-structure :reader data-structure)))

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
