;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defgeneric lazy-reference (input shape transformation)
  (:argument-precedence-order transformation shape input))

;;; Optimization:  Compose consecutive references.
(defmethod lazy-reference ((lazy-reference lazy-reference)
                           (shape shape)
                           (transformation transformation))
  (lazy-reference
   (input lazy-reference)
   shape
   (compose-transformations
    (transformation lazy-reference)
    transformation)))

;;; Optimization:  Drop references with no effect.
(defmethod lazy-reference ((lazy-array lazy-array)
                           (shape shape)
                           (identity-transformation identity-transformation))
  (if (and (shape-equal (shape lazy-array) shape)
           ;; Don't drop references to range immediates.  The reason for
           ;; this is that we never want these immediates to appear as
           ;; roots of a data flow graph.
           (not (typep lazy-array 'range-immediate)))
      lazy-array
      (call-next-method)))

;;; Handle empty shapes.
(defmethod lazy-reference ((lazy-array lazy-array)
                           (null null)
                           (transformation transformation))
  (empty-array))

;;; Default:  Construct a new reference.
(defmethod lazy-reference ((lazy-array lazy-array)
                           (shape shape)
                           (transformation transformation))
  (make-instance 'lazy-reference
    :ntype (element-ntype lazy-array)
    :inputs (list lazy-array)
    :shape shape
    :transformation transformation))

;;; Error handling.
(defmethod lazy-reference :before
    ((lazy-array lazy-array) (shape shape) (transformation transformation))
  (let ((relevant-shape (transform shape transformation))
        (input-shape (shape lazy-array)))
    (unless (and (= (shape-rank relevant-shape) (shape-rank input-shape))
                 (subshapep relevant-shape input-shape))
      (error "~@<Invalid reference to ~S with shape ~S and transformation ~S.~:@>"
             lazy-array shape transformation))))
