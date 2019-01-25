;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

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

;;; A range immediate is a rank one array, where every element with index
;;; (I), has the value I.
(defclass range-immediate (immediate)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors

(defun make-range-immediate (range)
  (make-instance 'range-immediate
    :element-type `(integer ,(range-start range) ,(range-end range))
    :shape (make-shape range)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate immediate)

(defmethod inputs ((immediate immediate))
  '())

(defmethod coerce-to-strided-array ((array array))
  (make-instance 'array-immediate
    :element-type (array-element-type array)
    :shape (shape array)
    :storage array))

(defmethod coerce-to-strided-array ((object t))
  (let ((element-type (type-of object)))
    (make-instance 'array-immediate
      :element-type element-type
      :shape (make-shape)
      :storage (make-array '() :element-type element-type
                               :initial-element object))))

(defmethod print-object ((array-immediate array-immediate) stream)
  (print-unreadable-object (array-immediate stream :type t :identity t)
    (princ (storage array-immediate) stream)))

(defmethod print-object ((range-immediate range-immediate) stream)
  (print-unreadable-object (range-immediate stream :type t :identity t)
    (format stream ":SHAPE ~A" (shape range-immediate))))

(defmethod transform ((strided-array strided-array) (transformation transformation))
  (make-reference
   strided-array
   (transform (shape strided-array) transformation)
   (invert-transformation transformation)))
