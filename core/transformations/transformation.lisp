;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;; Petalisp transformations are a combination of the following five
;;; elementary operations:
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the indices
;;; (4) introducing ranks with a one element range
;;; (5) removing ranks
;;;
;;; In linear algebra lingo, we have
;;;
;;; (1) adding a vector
;;; (2) multiplying with a diagonal matrix
;;; (3) multiplying with a permutation matrix
;;; (4) multiplying with an identity matrix, but with some zero rows inserted and adding a vector
;;; (5) multiplying with an identity matrix, but with some rows removed
;;;
;;; The transformation protocol is inspired by that of McCLIM. The
;;; difference is that McCLIM transformations are functions from R^2 to
;;; R^2, while Petalisp transformations are functions from N^n to N^m.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric transformation-equal (transformation-1 transformation-2))

(defgeneric compose-transformations (g f))

(defgeneric invert-transformation (transformation))

(defgeneric input-rank (transformation))

(defgeneric output-rank (transformation))

;;; For each input of TRANSFORMATION, invoke FUNCTION with the input index
;;; and the corresponding input constraint, or null, if there is no input
;;; constraint for this input.
;;;
;;; If FROM-END is false, the input indices are traversed in ascending
;;; order.  Otherwise, they are traversed in descending order.
(defgeneric map-transformation-inputs (function transformation &key from-end))

;;; For each output of TRANSFORMATION, invoke FUNCTION with the output
;;; index, input index, the scaling and the offset of that output.
;;;
;;; An input index of NIL and a scaling of zero is used, if (and only if)
;;; the output is constant.
;;;
;;; If FROM-END is false, the output indices are traversed in ascending
;;; order.  Otherwise, they are traversed in descending order.
(defgeneric map-transformation-outputs (function transformation &key from-end))

;;; Given a transformation mapping from (i1 ... iN) to (j1 ... jM),
;;; return a transformation mapping from (i0 i1 ... iN iN+1) to
;;; ((+(* i0 SCALE) OFFSET) j1 ... jM).
(defgeneric enlarge-transformation (transformation scale offset))

;;; Reorder, scale and shift the given OBJECT according to TRANSFORMATION.
(defgeneric transform (object transformation)
  (:argument-precedence-order transformation object))

(defgeneric transform-axis (axis transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass transformation ()
  ())

;; Forward declaration of the primary transformation constructors, because
;; they will be referenced before being defined.
(declaim (ftype (function (&key (:input-rank array-rank)
                                (:output-rank array-rank)
                                (:input-mask sequence)
                                (:output-mask sequence)
                                (:offsets sequence)
                                (:scalings sequence)))
                make-transformation)
         (ftype (function (array-length))
                identity-transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate transformation)

(defmethod transformation-equal ((t1 transformation) (t2 transformation))
  (= (input-rank t1)
     (input-rank t2))
  (= (output-rank t1)
     (output-rank t2)))

(defmethod compose-transformations :before
    ((g transformation) (f transformation))
  (assert (= (output-rank f) (input-rank g))))

(defmethod transform ((sequence sequence) (transformation transformation))
  (assert (= (length sequence) (input-rank transformation))))

(defmethod transform-axis ((axis integer) (transformation transformation))
  (assert (< -1 axis (input-rank transformation))))

(defmethod print-object ((transformation transformation) stream)
  (let ((inputs '()))
    (map-transformation-inputs
     (lambda (input-index input-constraint)
       (if (null input-constraint)
           (push (format-symbol :keyword "I~D" input-index) inputs)
           (push input-constraint inputs)))
     transformation
     :from-end t)
    (princ `(τ ,inputs ,(transform inputs transformation)) stream)))
