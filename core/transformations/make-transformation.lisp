;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

(defun make-transformation
    (&key
       (input-mask nil input-mask-supplied-p)
       (output-mask nil output-mask-supplied-p)
       (offsets nil offsets-supplied-p)
       (scalings nil scalings-supplied-p)
       (input-rank nil input-rank-supplied-p)
       (output-rank nil output-rank-supplied-p))
  ;; Attempt to derive the input and output rank.
  (multiple-value-bind (input-rank output-rank)
      (labels ((two-value-fixpoint (f x1 x2)
                 (multiple-value-bind (y1 y2) (funcall f x1 x2)
                   (if (and (eql x1 y1)
                            (eql x2 y2))
                       (values x1 x2)
                       (two-value-fixpoint f y1 y2))))
               (narrow-input-and-output-rank (i o)
                 (values
                  (cond (i i)
                        (input-rank-supplied-p input-rank)
                        (input-mask-supplied-p (length input-mask))
                        (o o))
                  (cond (o o)
                        (output-rank-supplied-p output-rank)
                        (output-mask-supplied-p (length output-mask))
                        (offsets-supplied-p (length offsets))
                        (scalings-supplied-p (length scalings))
                        (i i)))))
        (two-value-fixpoint #'narrow-input-and-output-rank nil nil))
    (check-type input-rank array-rank)
    (check-type output-rank array-rank)
    ;; Canonicalize all sequence arguments.
    (multiple-value-bind (input-mask identity-input-mask-p)
        (canonicalize-input-mask input-mask input-mask-supplied-p input-rank)
      (declare (simple-vector input-mask)
               (boolean identity-input-mask-p))
      (multiple-value-bind (output-mask identity-output-mask-p)
          (canonicalize-output-mask output-mask output-mask-supplied-p output-rank input-rank)
        (declare (simple-vector output-mask)
                 (boolean identity-output-mask-p))
        (multiple-value-bind (scalings identity-scalings-p)
            (canonicalize-scalings scalings scalings-supplied-p output-rank)
          (declare (simple-vector scalings)
                   (boolean identity-scalings-p))
          (multiple-value-bind (offsets identity-offsets-p)
              (canonicalize-offsets offsets offsets-supplied-p output-rank)
            (declare (simple-vector offsets)
                     (boolean identity-offsets-p))
            (unless (or identity-scalings-p identity-input-mask-p)
              (loop for input-index across output-mask
                    for scaling across scalings
                    do (assert (or (zerop scaling) input-index))))
            (if (and (= input-rank output-rank)
                     identity-input-mask-p
                     identity-output-mask-p
                     identity-scalings-p
                     identity-offsets-p)
                (identity-transformation input-rank)
                ;; A transformation is invertible, if each unused argument
                ;; has a corresponding input constraint.
                (if (loop for constraint across input-mask
                          for input-index from 0
                          always (or constraint (find input-index output-mask)))
                    (make-instance 'hairy-invertible-transformation
                      :input-rank input-rank
                      :output-rank output-rank
                      :input-mask input-mask
                      :output-mask output-mask
                      :scalings scalings
                      :offsets offsets)
                    (make-instance 'hairy-transformation
                      :input-rank input-rank
                      :output-rank output-rank
                      :input-mask input-mask
                      :output-mask output-mask
                      :scalings scalings
                      :offsets offsets)))))))))

(defun canonicalize-input-mask (value supplied-p input-rank)
  (if (not supplied-p)
      (values (make-sequence 'simple-vector input-rank :initial-element nil) t)
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) input-rank))
        (loop for element across vector
              do (assert (typep element '(or rational null)))
              unless (eql element 0) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun canonicalize-output-mask (value supplied-p output-rank input-rank)
  (if (not supplied-p)
      (let ((vector (make-sequence 'simple-vector output-rank :initial-element nil)))
        (loop for index below (min input-rank output-rank) do
          (setf (svref vector index) index))
        (values vector (= input-rank output-rank)))
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) output-rank))
        (loop for index below output-rank
              for element across vector
              do (assert (typep element '(or array-rank null)))
              unless (eql element index) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun canonicalize-scalings (value supplied-p output-rank)
  (if (not supplied-p)
      (values (make-sequence 'simple-vector output-rank :initial-element 1))
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) output-rank))
        (loop for element across vector
              do (assert (rationalp element))
              unless (eql element 1) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun canonicalize-offsets (value supplied-p output-rank)
  (if (not supplied-p)
      (values (make-sequence 'simple-vector output-rank :initial-element 0) t)
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) output-rank))
        (loop for element across vector
              do (assert (rationalp element))
              unless (eql element 0) do
                (setf identity-p nil))
        (values vector identity-p))))
