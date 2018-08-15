;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric immediatep (object))

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
  ((%range :initarg range :reader range)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod immediatep ((object non-immediate))
  nil)

(defmethod immediatep ((immediate immediate))
  t)

(defmethod inputs ((immediate immediate))
  '())

(defmethod make-strided-array ((array array))
  (if (zerop (array-rank array))
      (make-instance 'scalar-immediate
        :element-type (atomic-type (array-element-type array))
        :shape (load-time-value (make-shape '()))
        :storage (aref array))
      (make-instance 'array-immediate
        :element-type (atomic-type (array-element-type array))
        :shape (make-shape (array-dimensions array))
        :storage array)))

(defmethod make-strided-array ((object t))
  (make-instance 'scalar-immediate
    :element-type (atomic-type (type-of object))
    :shape (load-time-value (make-shape '()))
    :storage object))

(defmethod print-object ((immediate immediate) stream)
  (print-unreadable-object (immediate stream :type t :identity t)
    (princ (storage immediate) stream)))

(defmethod transform ((strided-array strided-array) (transformation transformation))
  (make-reference
   strided-array
   (transform (shape strided-array) transformation)
   (invert-transformation transformation)))
