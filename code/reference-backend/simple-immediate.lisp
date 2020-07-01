;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.reference-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

;;; A simple immediate is implemented as an EQUAL hash table that maps each
;;; index tuple to the corresponding value.
(defclass simple-immediate (non-empty-immediate)
  ((%table :initarg :table :reader table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions

(defun make-simple-immediate (shape element-type value-fn)
  (let ((table (make-hash-table :test #'equal)))
    (map-shape
     (lambda (index)
       (let ((value (funcall value-fn index)))
         (assert (typep value element-type))
         (setf (gethash index table) value)))
     shape)
    (make-instance 'simple-immediate
      :shape shape
      :ntype (petalisp.type-inference:ntype element-type)
      :table table)))

(defun iref (simple-immediate index)
  (multiple-value-bind (value present-p)
      (gethash index (table simple-immediate))
    (unless present-p
      (error "Invalid index ~S for the strided array ~S."
             index simple-immediate))
    value))

(defmethod lisp-datum-from-immediate ((simple-immediate simple-immediate))
  (let ((array (lisp-array-from-simple-immediate simple-immediate)))
    (if (zerop (array-rank array))
        (aref array)
        array)))

(defun lisp-array-from-simple-immediate (simple-immediate)
  (with-accessors ((shape shape)
                   (table table)
                   (element-type element-type))
      simple-immediate
    (let ((array (make-array (mapcar #'range-size (shape-ranges shape))
                             :element-type element-type)))
      (map-shape
       (lambda (index)
         (setf (apply #'aref array index)
               (gethash index table)))
       shape)
      array)))

(defmethod replace-lazy-array ((instance lazy-array) (replacement simple-immediate))
  (change-class instance 'array-immediate
    :storage (lisp-array-from-simple-immediate replacement)))
