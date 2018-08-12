;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; Reductions.  There is a whole zoo of possible reductions out there,
;;; like left folds, left folds with an initial value, left folds with an
;;; extra function to lift the first element of the sequence, right folds,
;;; accumulations in arbitrary order, accumulations with an initial value,
;;; simultaneous accumulation of several quantities and, finally,
;;; reductions of several sequences at once.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-reduction (f g a order)
  (:method-combination optimizing-constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reduction (non-immediate)
  ((%operator :initarg :operator :reader operator)))

(defclass tree-fold (reduction)
  ())

(defclass directed-fold (reduction)
  ((%initialization :initarg :initialization :reader initialization)))

(defclass left-fold (directed-fold)
  ())

(defclass right-fold (directed-fold)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-reduction :check (f g (a strided-array) order)
  (declare (ignore f g order))
  (demand (plusp (dimension a))
    "~@<Can only reduce data structures with dimension greater than zero.~:@>"))

(defmethod make-reduction (binary-operator unary-operator
                           (strided-array strided-array)
                           order)
  (multiple-value-bind (unary-element-type unary-designator)
      (dx-let ((argument-types (list (element-type strided-array))))
        (infer-type unary-operator argument-types))
    (multiple-value-bind (element-type binary-designator)
        (dx-let ((argument-types (list (element-type strided-array) unary-element-type)))
          (infer-type binary-operator argument-types))
      (make-instance 'tree-fold
        :operator binary-designator
        :initialization unary-designator
        :element-type element-type
        :inputs (list strided-array)
        :shape (shape-from-ranges (cdr (ranges (shape strided-array))))))))
