;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(declaim (ftype (function (lazy-array rank t) (values lazy-array rank &optional))
                lazy-reshape-aux
                lazy-reshape-aux/integer
                lazy-reshape-aux/transformation
                lazy-reshape-aux/function
                lazy-reshape-aux/shape))

(defun lazy-reshape (array &rest modifiers)
  (let* ((L (lazy-array array))
         (K (lazy-array-rank L)))
    (dolist (M modifiers L)
      (multiple-value-setq (L K)
        (lazy-reshape-aux L K M)))))

(defun lazy-reshape-aux (lazy-array n-axes modifier)
  "An auxiliary function for LAZY-RESHAPE that processes a single modifier,
and returns both the new lazy array and the new number of relevant axes
obtained after applying the modifier."
  (let* ((shape (lazy-array-shape lazy-array))
         (rank (shape-rank shape)))
    (assert (<= n-axes rank))
    (typecase modifier
      ;; Case 2: Reshape with an integer.
      (integer
       (lazy-reshape-aux/integer lazy-array n-axes modifier))
      ;; Case 2: Reshape with a transformation.
      (transformation
       (lazy-reshape-aux/transformation lazy-array n-axes modifier))
      ;; Case 3: Reshape with a function
      (function
       (lazy-reshape-aux/function lazy-array n-axes modifier))
      ;; Case 4: Reshape with a shape designator.
      (otherwise
       (lazy-reshape-aux/shape lazy-array n-axes (shape-designator-shape modifier))))))

(defun lazy-reshape-aux/integer (lazy-array n-axes integer)
  (declare (lazy-array lazy-array)
           (rank n-axes)
           (integer integer))
  (cond ((<= 0 integer n-axes)
         (values lazy-array integer))
        ((< n-axes integer array-rank-limit)
         (let* ((rank (lazy-array-rank lazy-array))
                (growth (- integer n-axes))
                (output-mask (make-array (+ rank growth) :initial-element nil)))
           (loop for axis below rank do
             (setf (aref output-mask (+ axis growth)) axis))
           (values
            (lazy-reshape-using-transformation
             lazy-array
             (make-transformation
              :input-rank rank
              :output-mask output-mask))
            integer)))
        ((> integer array-rank-limit)
         (error "~@<The integer modifier ~D exceeds the rank limit ~D.~:@>"
                integer array-rank-limit))
        (t
         (error "~@<The integer ~D isn't a valid reshape modifier.~:@>"
                integer))))

(defun lazy-reshape-aux/transformation (lazy-array n-axes transformation)
  (declare (transformation transformation))
  (unless (<= (transformation-input-rank transformation) n-axes)
    (error "~@<Cannot reshape ~R relevant axes of the lazy array ~S ~
               with the transformation ~S that has input rank ~R.~:@>"
           n-axes lazy-array transformation (transformation-input-rank transformation)))
  (values
   (lazy-reshape-using-transformation lazy-array transformation)
   (transformation-output-rank transformation)))

(defun lazy-reshape-aux/function (lazy-array n-axes function)
  (declare (function function))
  (let* ((shape (shape-subseq (lazy-array-shape lazy-array) 0 n-axes))
         (modifiers (multiple-value-list (funcall function shape)))
         (L lazy-array)
         (K n-axes))
    (dolist (M modifiers)
      (multiple-value-setq (L K)
        (lazy-reshape-aux L K M)))
    (values L K)))

(defun lazy-reshape-aux/shape (lazy-array n-axes shape)
  (declare (rank n-axes) (lazy-array lazy-array) (shape shape))
  (let* ((source-shape (lazy-array-shape lazy-array))
         (source-size (shape-size source-shape))
         (source-rank (shape-rank source-shape))
         (source-ranges (shape-ranges source-shape))
         (target-shape (make-shape (append (shape-ranges shape) (subseq source-ranges n-axes))))
         (target-size (shape-size target-shape)))
    (values
     (cond
       ;; Case 4a: Select a subset of elements.
       ((< target-size source-size)
        (unless (= n-axes (shape-rank shape))
          (error "~@<When selecting values, the number of relevant axes must be ~
                    equal to the rank of the shape modifier.  Here, the number of ~
                    relevant axes is ~R, and the shape modifier ~S has rank ~R.~:@>"
                 n-axes shape (shape-rank shape)))
        (lazy-ref
         lazy-array
         target-shape
         (identity-transformation source-rank)))
       ;; Case 4b: Change the index scheme while preserving the original
       ;; lexicographic ordering.
       ((= target-size source-size)
        (lazy-change-shape lazy-array target-shape))
       ;; Case 4c: Broadcast values of the input.
       ((> target-size source-size)
        (lazy-ref
         lazy-array
         target-shape
         (make-broadcasting-transformation source-shape target-shape n-axes))))
     (shape-rank shape))))
