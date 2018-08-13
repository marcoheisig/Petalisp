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

(defgeneric make-reduction (operator inputs)
  (:method-combination optimizing-constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reduction (non-immediate)
  ((%operator :initarg :operator :reader operator)
   (%value-n :initarg :value-n :reader value-n)
   (%conditions :initarg :conditions :reader conditions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-reduction :check (operator inputs)
  (declare (ignore operator))
  (unless (identical inputs :test #'set-equal :key #'shape)
    (error "~@<Can only reduce data structures of equal shape.~:@>"))
  (unless (plusp (dimension (shape (first inputs))))
    (error "~@<Can only reduce data structures with dimension greater than zero.~:@>")))

(defmethod make-reduction ((operator function) (inputs list))
  (let ((input-types (mapcar #'element-type inputs)))
    (multiple-value-bind (result-types more-p conditions operator-name)
        (infer-type operator (append input-types input-types))
      (declare (ignore more-p))
      (unless (= (length result-types) (length input-types))
        (error "~@<The function ~S returns ~S argument~:P, but is expected ~
                     to return ~S arguments.~:@>"
               operator (length result-types) (length input-types)))
      (values-list
       (loop for result-type in result-types
             for value-n from 0
             collect
             (make-instance 'reduction
               :operator (or operator-name operator)
               :value-n value-n
               :conditions conditions
               :element-type result-type
               :inputs inputs
               :shape (shape-from-ranges (cdr (ranges (shape (first inputs)))))))))))
