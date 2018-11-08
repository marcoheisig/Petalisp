;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-application (operator inputs)
  (:method-combination optimizing-constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass application (non-immediate)
  ((%operator :initarg :operator :reader operator :type (or symbol function))
   (%value-n :initarg :value-n :reader value-n :type (integer 0 #.multiple-values-limit))
   (%conditions :initarg :conditions :reader conditions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-application :check ((function function) (inputs list))
  (assert (identical inputs :test #'set-equal :key #'shape)))

(defmethod make-application ((function function) inputs)
  (multiple-value-bind (result-types more-p conditions function-name)
      (infer-type function (mapcar #'element-type inputs))
    (let ((element-types (if (and more-p (null result-types))
                             '(t)
                             result-types)))
      (values-list
       (loop for element-type in element-types
             for value-n from 0
             collect
             (make-instance 'application
               :operator (or function-name function)
               :value-n value-n
               :conditions conditions
               :element-type element-type
               :inputs inputs
               :shape (shape (first inputs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant Folding
;;;
;;; In Petalisp, constant folding works somewhat differently, because all
;;; data is constant by definition.  A greedy constant folding algorithm
;;; would directly compute the roots of the scheduled data flow graph.
;;; However, since constant folding happens in the high level program, this
;;; would completely sidestep the scheduler and associated optimization.
;;;
;;; The only case where serial and parallel execution are equally fast is
;;; for scalar values.  Our conservative choice is therefore to fold only
;;; applications to scalar values.  Scalar values can be either immediates
;;; of size one, or references to such immediates.  Note, however, that
;;; such references need themselves not be of size one -- they may also be
;;; broadcasting references.

(defmethod make-application :optimize ((function function) (inputs list))
  (block nil
    (labels ((fail () (return))
             (value-or-fail (strided-array)
               (typecase strided-array
                 (array-immediate
                  (if (= 1 (total-size (shape strided-array)))
                      (aref (storage strided-array))
                      (fail)))
                 (reference
                  (value-or-fail (input strided-array)))
                 (t (fail)))))
      (reshape (apply function (mapcar #'value-or-fail inputs))
               (shape (first inputs))))))
