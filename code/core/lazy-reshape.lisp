;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defgeneric lazy-reshape (input shape transformation)
  (:argument-precedence-order transformation shape input))

;;; Ensure that the special variable *RELEVANT-SHAPE* carries the value of
;;; the referenced input shape within each call to LAZY-RESHAPE.
(defvar *relevant-shape*)

(defmethod lazy-reshape :around
    ((lazy-array lazy-array)
     (shape shape)
     (transformation transformation))
  (let ((*relevant-shape* (transform shape transformation)))
    (call-next-method)))

;;; Error handling.
(defmethod lazy-reshape :before
    ((lazy-array lazy-array)
     (shape shape)
     (transformation transformation))
  (let ((input-shape (shape lazy-array)))
    (unless (and (= (shape-rank *relevant-shape*)
                    (shape-rank input-shape))
                 (subshapep *relevant-shape* input-shape))
      (error "~@<Invalid reference to ~S with shape ~S and transformation ~S.~:@>"
             lazy-array shape transformation))))

;;; Optimization: Compose consecutive references.
(defmethod lazy-reshape
    ((lazy-reshape lazy-reshape)
     (shape shape)
     (transformation transformation))
  (lazy-reshape
   (input lazy-reshape)
   shape
   (compose-transformations
    (transformation lazy-reshape)
    transformation)))

;;; Optimization: Drop references with no effect.
(defmethod lazy-reshape
    ((lazy-array lazy-array)
     (shape shape)
     (identity-transformation identity-transformation))
  (if (and (shape-equal (shape lazy-array) shape)
           ;; Don't drop references to range immediates.  The reason for
           ;; this is that we never want these immediates to appear as
           ;; roots of a data flow graph.
           (not (typep lazy-array 'range-immediate)))
      lazy-array
      (call-next-method)))

;;; Optimization: Skip references to lazy fuse operations in case they fall
;;; entirely within a single input of that fusion.
(defmethod lazy-reshape
    ((lazy-fuse lazy-fuse)
     (shape shape)
     (transformation transformation))
  (loop for input in (inputs lazy-fuse)
        when (subshapep *relevant-shape* (shape input)) do
          (return-from lazy-reshape
            (lazy-reshape input shape transformation)))
  (call-next-method))

;;; Handle empty shapes.
(defmethod lazy-reshape
    ((lazy-array lazy-array)
     (null null)
     (transformation transformation))
  (empty-array))

;;; Default:  Construct a new reference.
(defmethod lazy-reshape ((lazy-array lazy-array)
                        (shape shape)
                        (transformation transformation))
  (make-instance 'lazy-reshape
    :ntype (element-ntype lazy-array)
    :inputs (list lazy-array)
    :shape shape
    :transformation (add-transformation-constraints shape transformation)))

(defun add-transformation-constraints (shape transformation)
  (if (loop for range in (shape-ranges shape)
            for mask-entry across (transformation-input-mask transformation)
            never (and (size-one-range-p range)
                       (not mask-entry)))
      transformation
      (let ((input-mask (copy-seq (transformation-input-mask transformation))))
        (loop for range in (shape-ranges shape)
              for index from 0
              when (size-one-range-p range)
                do (setf (aref input-mask index)
                         (range-start range)))
        (compose-transformations
         transformation
         (make-transformation :input-mask input-mask)))))

