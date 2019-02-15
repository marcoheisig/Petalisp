;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-application (value-n operator inputs)
  (:method-combination petalisp.utilities:optimizing-constructor))

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

(defmethod make-application :check ((value-n integer) (function function) (inputs list))
  (assert (<= 0 value-n (1- multiple-values-limit)))
  (assert (petalisp.utilities:identical inputs :test #'set-equal :key #'shape)))

(defmethod make-application ((value-n integer) (function function) inputs)
  (multiple-value-bind (element-types more-p conditions function-name)
      (infer-type function (mapcar #'element-type inputs))
    (make-instance 'application
      :operator (or function-name function)
      :value-n value-n
      :conditions conditions
      :element-type (or (nth value-n element-types) t)
      :inputs inputs
      :shape (shape (first inputs)))))

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

(defun broadcasting-transformation (shape)
  (make-transformation
   :output-rank 0
   :input-rank (rank shape)))

(defmethod make-application :optimize ((value-n integer) (function function) (inputs list))
  (block nil
    (labels ((fail () (return))
             (value-or-fail (lazy-array)
               (typecase lazy-array
                 (array-immediate
                  (if (= 1 (total-size (shape lazy-array)))
                      (row-major-aref (storage lazy-array) 0)
                      (fail)))
                 (reference
                  (value-or-fail (input lazy-array)))
                 (t (fail)))))
      (let ((value (nth-value value-n (apply function (mapcar #'value-or-fail inputs))))
            (shape (shape (first inputs))))
        (make-reference
         (coerce-to-lazy-array value)
         shape
         (broadcasting-transformation shape))))))
