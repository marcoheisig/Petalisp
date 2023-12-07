;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-reshape (array &rest modifiers)
  (let ((lazy-array (lazy-array array)))
    (dolist (modifier modifiers lazy-array)
      (setf lazy-array (lazy-reshape-aux lazy-array modifier)))))

(defun lazy-reshape-aux (lazy-array modifier)
  "An auxiliary function for LAZY-RESHAPE that processes a single modifier."
  (declare (lazy-array lazy-array))
  (typecase modifier
    (transformation
     (lazy-reshape-using-transformation lazy-array modifier))
    (function
     (lazy-reshape-using-function lazy-array modifier))
    (otherwise
     (lazy-reshape-using-shape lazy-array (shape-designator-shape modifier)))))

(defun lazy-reshape-using-transformation (lazy-array transformation)
  (declare (lazy-array lazy-array)
           (transformation transformation))
  (let ((lrank (lazy-array-rank lazy-array))
        (trank (transformation-input-rank transformation)))
    (cond
      ;; Make sure the lazy array has at least the same rank as expected by the
      ;; transformation.
      ((< lrank trank)
       (setf lazy-array
             (lazy-reshape-using-transformation
              lazy-array
              (make-transformation :input-rank lrank :output-rank trank))))
      ;; Make sure the transformation has the same rank as the lazy array.
      ((> lrank trank)
       (setf transformation
             (inflate-transformation transformation (- lrank trank))))))
  (lazy-ref
   lazy-array
   (transform-shape (lazy-array-shape lazy-array) transformation)
   (invert-transformation transformation)))

(defun lazy-reshape-using-function (lazy-array function)
  (declare (lazy-array lazy-array)
           (function function))
  (multiple-value-call #'lazy-reshape
    lazy-array
    (funcall function (lazy-array-shape lazy-array))))

(defun lazy-reshape-using-shape (lazy-array shape)
  (declare (lazy-array lazy-array)
           (shape shape))
  (let* ((shape-rank (shape-rank shape))
         (source-shape (lazy-array-shape lazy-array))
         (source-rank (shape-rank source-shape))
         (target-shape
           (if (< source-rank shape-rank)
               shape
               (make-shape
                (append (shape-ranges shape)
                        (subseq (shape-ranges source-shape) shape-rank))))))
    (lazy-ref
     lazy-array
     target-shape
     (make-reshape-transformation target-shape source-shape))))

(defun make-reshape-transformation (target-shape source-shape)
  "Returns a transformation that maps every element of the supplied target shape to an
element of the supplied source shape.  Signals an error if the supplied shapes
don't have the same rank."
  ;; The terminology is a bit confusing here because the transformation
  ;; argument of LAZY-REF points "backwards", i.e., from values of the target
  ;; to values of the source.
  (declare (shape source-shape target-shape))
  (let* ((input-ranges (shape-ranges target-shape))
         (input-rank (shape-rank target-shape))
         (output-ranges (shape-ranges source-shape))
         (output-rank (shape-rank source-shape))
         (input-mask (make-array input-rank :initial-element nil))
         (output-mask (make-array output-rank :initial-element nil))
         (scalings (make-array output-rank :initial-element 1))
         (offsets (make-array output-rank :initial-element 0)))
    (unless (<= output-rank input-rank)
      (error "~@<Cannot broadcast from rank ~R to rank ~R.~:@>"
             output-rank input-rank))
    ;; Determine the input mask.
    (loop for axis below input-rank
          for input-range in input-ranges
          do (when (range-with-size-one-p input-range)
               (setf (aref input-mask axis)
                     (range-start input-range))))
    ;; Process all axes up OUTPUT-RANK.
    (loop for axis from 0
          for input-range in input-ranges
          for output-range in output-ranges
          do (cond
               ;; Move.
               ((and (= (range-size output-range)
                        (range-size input-range))
                     (not (range-emptyp input-range)))
                (let* ((a (/ (range-step output-range)
                             (range-step input-range)))
                       (b (- (range-start output-range)
                             (* (range-start input-range) a))))
                  (setf (aref output-mask axis) axis)
                  (setf (aref scalings axis) a)
                  (setf (aref offsets axis) b)))
               ;; Broadcast.
               ((range-with-size-one-p output-range)
                (setf (aref scalings axis)
                      0)
                (setf (aref offsets axis)
                      (range-start output-range)))
               ;; Select.
               ((subrangep input-range output-range)
                (setf (aref output-mask axis)
                      axis))
               (t
                (error "~@<Cannot reshape the range ~S in axis ~D to the range ~S.~:@>"
                       output-range axis input-range))))
    (make-transformation
     :input-mask input-mask
     :output-mask output-mask
     :scalings scalings
     :offsets offsets)))

(defun lazy-ref (input shape transformation)
  "Returns a lazy array whose contents are a selection of the supplied lazy
array, which has the supplied shape, and whose mapping from its element to that
of its input is the supplied transformation."
  (declare (lazy-array input)
           (shape shape)
           (transformation transformation))
  (let ((relevant-shape (transform-shape shape transformation))
        (input-shape (lazy-array-shape input)))
    (unless (and (= (shape-rank relevant-shape)
                    (shape-rank input-shape))
                 (subshapep relevant-shape input-shape))
      (error "~@<Invalid reference to ~S with shape ~S and transformation ~S.~:@>"
             input shape transformation))
    (labels
        ((ref (lazy-array relevant-shape transformation)
           (declare (lazy-array lazy-array)
                    (shape relevant-shape)
                    (transformation transformation))
           ;; Optimization: Turn empty references into array immediates.
           (when (shape-emptyp shape)
             (return-from ref
               (empty-lazy-array shape)))
           ;; Optimization: Drop references with no effect.
           (when (and (transformation-identityp transformation)
                      (shape= (lazy-array-shape lazy-array) shape))
             (return-from ref lazy-array))
           ;; Optimization: Compose consecutive lazy reshapes.
           (when (delayed-reshape-p (lazy-array-delayed-action lazy-array))
             (let ((delayed-reshape (lazy-array-delayed-action lazy-array)))
               (return-from ref
                 (ref
                  (delayed-reshape-input delayed-reshape)
                  (transform-shape
                   relevant-shape
                   (delayed-reshape-transformation delayed-reshape))
                  (compose-transformations
                   (delayed-reshape-transformation delayed-reshape)
                   transformation)))))
           ;; Optimization: Skip references to lazy fuse operations in case
           ;; they fall entirely within a single input of that fusion.
           (when (delayed-fuse-p (lazy-array-delayed-action lazy-array))
             (let ((delayed-fuse (lazy-array-delayed-action lazy-array)))
               (loop for input in (delayed-fuse-inputs delayed-fuse)
                     when (subshapep relevant-shape (lazy-array-shape input)) do
                       (return-from ref
                         (ref input relevant-shape transformation)))))
           ;; Default: Construct a new reference.
           (make-lazy-array
            :shape shape
            :ntype (lazy-array-ntype lazy-array)
            :depth (1+ (lazy-array-depth lazy-array))
            :delayed-action
            (make-delayed-reshape
             :input lazy-array
             :transformation (add-transformation-constraints shape transformation)))))
      (ref input relevant-shape transformation))))

;;; We can turn each axis of the resulting shape that consists of a single
;;; element into an additional input constraint for the transformation.
;;; This augmentation is important, because the additional constraints can
;;; turn a previously non-invertible transformation invertible.
(defun add-transformation-constraints (shape transformation)
  (if (loop for range in (shape-ranges shape)
            for mask-entry across (transformation-input-mask transformation)
            never (and (range-with-size-one-p range)
                       (not mask-entry)))
      transformation
      (let ((input-mask (copy-seq (transformation-input-mask transformation))))
        (loop for range in (shape-ranges shape)
              for index from 0
              when (range-with-size-one-p range)
                do (setf (aref input-mask index)
                         (range-start range)))
        (compose-transformations
         transformation
         (make-transformation :input-mask input-mask)))))
