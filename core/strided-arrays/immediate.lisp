;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass immediate (strided-array)
  ())

(defclass non-immediate (strided-array)
  ((%inputs :initarg :inputs :reader inputs)))

(defclass scalar-immediate (immediate)
  ((%storage :initarg :storage :reader storage)))

(defclass array-immediate (immediate)
  ((%storage :initarg :storage :reader storage)))

(defclass range-immediate (immediate)
  ((%axis :initarg :axis :reader axis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors

(defun make-scalar-immediate (scalar)
  (make-instance 'scalar-immediate
    :element-type (type-of scalar)
    :shape (load-time-value (make-shape '()))
    :storage scalar))

(defun make-array-immediate (array)
  (if (zerop (array-rank array))
      (make-scalar-immediate (aref array))
      (make-instance 'array-immediate
        :element-type (array-element-type array)
        :shape (make-shape (array-dimensions array))
        :storage array)))

(defun make-range-immediate (shape axis)
  (make-instance 'range-immediate
    :element-type 'integer
    :shape shape
    :axis axis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate immediate)

(defmethod inputs ((immediate immediate))
  '())

(defmethod coerce-to-strided-array ((array array))
  (make-array-immediate array))

(defmethod coerce-to-strided-array ((object t))
  (make-scalar-immediate object))

(defmethod print-object ((immediate immediate) stream)
  (print-unreadable-object (immediate stream :type t :identity t)
    (princ (storage immediate) stream)))

(defmethod print-object ((range-immediate range-immediate) stream)
  (print-unreadable-object (range-immediate stream :type t :identity t)
    (format stream ":AXIS ~D" (axis range-immediate))))

(defmethod transform ((strided-array strided-array) (transformation transformation))
  (make-reference
   strided-array
   (transform (shape strided-array) transformation)
   (invert-transformation transformation)))
