;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(declaim (ftype (function (rank lazy-array t) (values rank lazy-array &optional))
                lazy-reshape-aux lazy-reshape-aux/shape))

(defun lazy-reshape (array &rest modifiers)
  (let* ((lazy-array (lazy-array array))
         (n-axes (lazy-array-rank lazy-array)))
    (dolist (modifier modifiers lazy-array)
      (multiple-value-setq (n-axes lazy-array)
        (lazy-reshape-aux n-axes lazy-array modifier)))))

(defun lazy-reshape-aux (n-axes lazy-array modifier)
  "An auxiliary function for LAZY-RESHAPE that processes a single modifier,
and returns both the new number of relevant axes and the lazy array
obtained after applying the modifier."
  (declare (rank n-axes) (lazy-array lazy-array))
  (let* ((shape (lazy-array-shape lazy-array))
         (rank (shape-rank shape)))
    (unless (<= n-axes rank)
      (error "~@<Cannot reshape ~R axes of the array ~A with rank ~R.~:@>"
             n-axes lazy-array rank))
    (typecase modifier
      ;; Case 1: Reshape with a non-negative integer.
      (integer
       (check-type modifier rank "a valid array rank")
       (unless (<= modifier rank)
         (error "~@<Invalid rank modifier ~D for the array ~S with rank ~D.~:@>"
                modifier lazy-array rank))
       (values modifier lazy-array))
      ;; Case 2: Reshape with a transformation.
      (transformation
       (unless (<= (transformation-input-rank modifier) n-axes)
         (error "~@<Cannot reshape ~R relevant axes of the lazy array ~S ~
                    with the transformation ~S that has input rank ~R.~:@>"
                n-axes lazy-array modifier (transformation-input-rank modifier)))
       (values
        (transformation-output-rank modifier)
        (lazy-reshape-using-transformation lazy-array modifier)))
      ;; Case 2: Reshape using a shape function.
      (function
       (dolist (modifier (multiple-value-list (funcall modifier (subshape shape 0 n-axes))))
         (multiple-value-setq (n-axes lazy-array)
           (lazy-reshape-aux n-axes lazy-array modifier)))
       (values n-axes lazy-array))
      ;; Case 3: Reshape using a shape designator.
      (otherwise
       (lazy-reshape-aux/shape n-axes lazy-array (shape-designator-shape modifier))))))

(defun lazy-reshape-aux/shape (n-axes lazy-array shape)
  (declare (rank n-axes) (lazy-array lazy-array) (shape shape))
  (let* ((source-shape (lazy-array-shape lazy-array))
         (source-size (shape-size source-shape))
         (source-rank (shape-rank source-shape))
         (source-ranges (shape-ranges source-shape))
         (target-shape (make-shape (append (shape-ranges shape) (subseq source-ranges n-axes))))
         (target-size (shape-size target-shape)))
    (values
     (shape-rank shape)
     (cond
       ;; Case 3a: Select a subset of elements.
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
       ;; Case 3b: Change the index scheme while preserving the original
       ;; lexicographic ordering.
       ((= target-size source-size)
        (lazy-change-shape lazy-array target-shape))
       ;; Case 3c: Broadcast values of the input.
       ((> target-size source-size)
        (lazy-ref
         lazy-array
         target-shape
         (make-broadcasting-transformation source-shape target-shape n-axes)))))))
