;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass immediate (strided-array)
  ())

(defclass non-immediate (strided-array)
  ((%inputs :initarg :inputs :reader inputs)))

;;; An array immediate is a strided array whose elements reside directly in
;;; a Lisp array.
(defclass array-immediate (immediate)
  ((%storage :initarg :storage :reader storage)))

;;; A scalar immediate is equivalent to an array immediate with rank zero.
;;; By introducing a separate class for this common case, we avoid boxing
;;; scalars as arrays and we can dispatch specifically on scalars.
(defclass scalar-immediate (immediate)
  ((%storage :initarg :storage :reader storage)))

;;; A range immediate is a rank one array, where every element with index
;;; (I), has the value I.
(defclass range-immediate (immediate)
  ())

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

(defun make-range-immediate (range)
  (make-instance 'range-immediate
    :element-type `(integer ,(range-start range) ,(range-end range))
    :shape (shape-from-ranges (list range))))

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

(defmethod print-object ((array-immediate array-immediate) stream)
  (print-unreadable-object (array-immediate stream :type t :identity t)
    (princ (storage array-immediate) stream)))

(defmethod print-object ((scalar-immediate scalar-immediate) stream)
  (print-unreadable-object (scalar-immediate stream :type t :identity t)
    (princ (storage scalar-immediate) stream)))

(defmethod print-object ((range-immediate range-immediate) stream)
  (print-unreadable-object (range-immediate stream :type t :identity t)
    (format stream ":SHAPE ~A" (shape range-immediate))))

(defmethod transform ((strided-array strided-array) (transformation transformation))
  (make-reference
   strided-array
   (transform (shape strided-array) transformation)
   (invert-transformation transformation)))
