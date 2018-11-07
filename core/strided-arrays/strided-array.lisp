;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric coerce-to-strided-array (array)
  (:method-combination optimizing-constructor))

(defgeneric element-type (strided-array))

(defgeneric inputs (strided-array))

(defgeneric refcount (strided-array))

(defgeneric total-size (strided-array))

(defgeneric array-shape (strided-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass strided-array ()
  ((%element-type :initarg :element-type :reader element-type)
   (%shape :initarg :shape :reader array-shape)
   (%refcount :initform 0 :accessor refcount))
  (:default-initargs :element-type t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod coerce-to-strided-array ((strided-array strided-array))
  strided-array)

(defmethod total-size ((strided-array strided-array))
  (set-size (array-shape strided-array)))

(defmethod total-size ((finite-set finite-set))
  (set-size finite-set))

(defmethod initialize-instance :after ((strided-array strided-array)
                                       &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input)))
        (inputs strided-array)))

(defmethod rank ((strided-array strided-array))
  (rank (array-shape strided-array)))

(defun input (object)
  (destructuring-bind (input) (inputs object) input))

(defmethod print-object ((strided-array strided-array) stream)
  (print-unreadable-object (strided-array stream :type t)
    (format stream "~S ~S" (element-type strided-array) (array-shape strided-array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Treating Arrays as Strided Arrays

(defmethod total-size ((array array))
  (array-total-size array))

(defmethod array-shape ((array array))
  (shape-from-ranges
   (loop for axis below (array-rank array)
         collect
         (let ((dim (array-dimension array axis)))
           (make-range 0 1 (1- dim))))))

(defmethod element-type ((array array))
  (array-element-type array))

(defmethod rank ((array array))
  (array-rank array))
