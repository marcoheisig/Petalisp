;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-reference (strided-array shape transformation)
  (:method-combination optimizing-constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reference (non-immediate)
  ((%transformation :initarg :transformation :reader transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-reference :check
    ((strided-array strided-array)
     (shape shape)
     (transformation transformation))
  (let ((relevant-shape (transform shape transformation))
        (input-shape (shape strided-array)))
    (demand (and (= (rank relevant-shape) (rank input-shape))
                 (set-subsetp relevant-shape input-shape))
      "~@<The index shape referenced by the current reference is ~S, ~
          which is not a subshape of ~S, the index shape of the input of ~
          the current reference.~:@>"
      relevant-shape
      input-shape))
  (demand (= (rank shape) (input-rank transformation))
    "~@<The rank of the index shape of a reference operation must ~
        be equal to the input rank of its transformation.  The ~
        index shape ~S has the rank ~R, but the input rank ~
        of the transformation ~S is ~R.~:@>"
    shape
    (rank shape)
    transformation
    (input-rank transformation)))

;;; Combine consecutive references
(defmethod make-reference :optimize
    ((reference reference) (shape shape) (transformation transformation))
  (make-reference
   (input reference)
   shape
   (compose-transformations (transformation reference) transformation)))

;;; Drop references with no effect.
(defmethod make-reference :optimize
    ((strided-array strided-array) (shape shape) (identity-transformation identity-transformation))
  (when (set-equal (shape strided-array) shape)
    strided-array))

(defmethod make-reference
    ((strided-array strided-array) (shape shape) (transformation transformation))
  (make-instance 'reference
    :element-type (element-type strided-array)
    :inputs (list strided-array)
    :shape shape
    :transformation transformation))
