;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric strided-array (array)
  (:method-combination optimizing-constructor))

(defgeneric element-type (strided-array))

(defgeneric inputs (strided-array))

(defgeneric refcount (strided-array))

(defgeneric size (strided-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass strided-array ()
  ((%element-type :initarg :element-type :reader element-type)
   (%shape :initarg :shape :reader shape :reader shape)
   (%refcount :initform 0 :accessor refcount))
  (:default-initargs :element-type t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod strided-array ((strided-array strided-array))
  strided-array)

(defmethod size ((strided-array strided-array))
  (set-size (shape strided-array)))

(defmethod size ((array array))
  (array-total-size array))

(defmethod size ((finite-set finite-set))
  (set-size finite-set))

(defmethod initialize-instance :after ((strided-array strided-array)
                                       &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input)))
        (inputs strided-array)))

(defmethod dimension ((strided-array strided-array))
  (dimension (shape strided-array)))

(defmethod dimension ((array array))
  (array-rank array))

(defun input (object)
  (destructuring-bind (input) (inputs object) input))

(defmethod print-object ((strided-array strided-array) stream)
  (print-unreadable-object (strided-array stream :type t)
    (format stream "~S ~S" (element-type strided-array) (shape strided-array))))
