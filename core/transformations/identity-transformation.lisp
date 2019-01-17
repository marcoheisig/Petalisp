;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

(defclass identity-transformation (invertible-transformation)
  ((%rank :initarg :rank
          :reader input-rank
          :reader output-rank
          :type (integer 0 *))))

(defun identity-transformation (rank)
  (petalisp-memoization:with-vector-memoization (rank)
    (make-instance 'identity-transformation
      :rank rank)))

(define-class-predicate identity-transformation :hyphenate t)

(defmethod transformation-equal
    ((transformation-1 identity-transformation)
     (transformation-2 identity-transformation))
  (= (input-rank transformation-1)
     (input-rank transformation-2)))

(defmethod compose-transformations
    ((g identity-transformation) (f transformation))
  f)

(defmethod compose-transformations
    ((g transformation) (f identity-transformation))
  g)

(defmethod invert-transformation
    ((transformation identity-transformation))
  transformation)

(defmethod enlarge-transformation
    ((transformation identity-transformation) (scale (eql 1)) (offset (eql 0)))
  (identity-transformation (1+ (input-rank transformation))))

(defmethod enlarge-transformation
    ((transformation identity-transformation) (scale rational) (offset rational))
  (let* ((rank (1+ (input-rank transformation)))
         (offsets (make-array rank :initial-element 0))
         (scalings (make-array rank :initial-element 1)))
    (setf (aref offsets 0) offset)
    (setf (aref scalings 0) scale)
    (make-transformation
     :input-rank rank
     :output-rank rank
     :scalings scalings
     :offsets offsets)))

(defmethod map-transformation-inputs
    ((function function) (transformation identity-transformation) &key from-end)
  (if (not from-end)
      (loop for input-index below (input-rank transformation) do
        (funcall function input-index nil))
      (loop for input-index downfrom (1- (input-rank transformation)) to 0 do
        (funcall function input-index nil))))

(defmethod map-transformation-outputs
    ((function function) (transformation identity-transformation) &key from-end)
  (if (not from-end)
      (loop for index below (output-rank transformation) do
        (funcall function index index 1 0))
      (loop for index downfrom (1- (output-rank transformation)) to 0 do
        (funcall function index index 1 0))))

(defmethod transform ((sequence sequence) (operator identity-transformation))
  sequence)

(defmethod transform-axis ((axis integer) (transformation identity-transformation))
  axis)
