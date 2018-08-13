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

(defgeneric make-reduction (strategy operator initializer inputs)
  (:method-combination optimizing-constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reduction (non-immediate)
  ((%operator :initarg :operator :reader operator)
   (%value-n :initarg :value-n :reader value-n)
   (%conditions :initarg :conditions :reader conditions)))

(defclass tree-fold (reduction)
  ())

(defclass directed-fold (reduction)
  ((%initializer :initarg :initializer :reader initializer)))

(defclass left-fold (directed-fold)
  ())

(defclass right-fold (directed-fold)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-reduction :check (strategy operator initializer inputs)
  (declare (ignore strategy operator initializer))
  (unless (identical inputs :test #'set-equal :key #'shape)
    (error "~@<Can only reduce data structures of equal shape.~:@>"))
  (unless (plusp (dimension (shape (first inputs))))
    (error "~@<Can only reduce data structures with dimension greater than zero.~:@>")))

(defmethod make-reduction ((strategy (eql 'tree-fold))
                           (operator function)
                           (initializer null)
                           (inputs list))
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
             (make-instance 'tree-fold
               :operator (or operator-name operator)
               :value-n value-n
               :conditions conditions
               :element-type result-type
               :inputs inputs
               :shape (shape-from-ranges (cdr (ranges (shape (first inputs)))))))))))

(defmethod make-reduction ((strategy (eql 'left-fold))
                           (operator function)
                           (initializer function)
                           (inputs list))
  (let ((input-types (mapcar #'element-type inputs)))
    (multiple-value-bind (accumulation-types more-p initializer-conditions initializer-name)
        (infer-type initializer input-types)
      (declare (ignore more-p))
      (multiple-value-bind (result-types more-p operator-conditions operator-name)
          (infer-type operator (append accumulation-types input-types))
        (declare (ignore more-p))
        (unless (= (length result-types) (length accumulation-types))
          (error "~@<The function ~S returns ~S argument~:P, but is expected ~
                     to return ~S arguments.~:@>"
                 operator (length result-types) (length accumulation-types)))
        (values-list
         (loop for result-type in result-types
               for value-n from 0
               collect
               (make-instance 'left-fold
                 :operator (or operator-name operator)
                 :initializer (or initializer-name initializer)
                 :value-n value-n
                 :conditions (union operator-conditions initializer-conditions)
                 :element-type result-type
                 :inputs inputs
                 :shape (shape-from-ranges (cdr (ranges (shape (first inputs))))))))))))

(defmethod make-reduction ((strategy (eql 'right-fold))
                           (operator function)
                           (initializer function)
                           (inputs list))
  (let ((input-types (mapcar #'element-type inputs)))
    (multiple-value-bind (accumulation-types more-p initializer-conditions initializer-name)
        (infer-type initializer input-types)
      (declare (ignore more-p))
      (multiple-value-bind (result-types more-p operator-conditions operator-name)
          (infer-type operator (append input-types accumulation-types))
        (declare (ignore more-p))
        (unless (= (length result-types) (length accumulation-types))
          (error "~@<The function ~S returns ~S argument~:P, but is expected ~
                     to return ~S arguments.~:@>"
                 operator (length result-types) (length accumulation-types)))
        (values-list
         (loop for result-type in result-types
               for value-n from 0
               collect
               (make-instance 'right-fold
                 :operator (or operator-name operator)
                 :initializer (or initializer-name initializer)
                 :value-n value-n
                 :conditions (union operator-conditions initializer-conditions)
                 :element-type result-type
                 :inputs inputs
                 :shape (shape-from-ranges (cdr (ranges (shape (first inputs))))))))))))
