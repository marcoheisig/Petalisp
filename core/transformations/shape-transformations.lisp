;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric broadcasting-transformation (from-shape to-shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod transform :before ((shape shape) (transformation transformation))
  (demand (= (rank shape) (input-rank transformation))
    "~@<Cannot apply the transformation ~A with input rank ~R ~
        to the index shape ~A with rank ~R.~:@>"
    transformation (input-rank transformation)
    shape (rank shape))
  (when-let ((input-constraints (input-constraints transformation)))
    (loop for range in (ranges shape)
          for constraint across input-constraints
          for index from 0 do
            (unless (not constraint)
              (demand (and (= constraint (range-start range))
                           (= constraint (range-end range)))
                "~@<The ~:R rank of the shape ~W violates ~
                    the input constraint ~W of the transformation ~W.~:@>"
                index shape constraint transformation)))))

(defmethod transform ((shape shape) (operator identity-transformation))
  shape)

(defmethod transform ((shape shape) (transformation hairy-transformation))
  (let ((output-ranges (make-list (output-rank transformation)))
        (input-ranges (ranges shape)))
    (flet ((store-output-range (output-index input-index scaling offset)
             (setf (elt output-ranges output-index)
                   (if (not input-index)
                       (make-range offset 1 offset)
                       (let ((input-range (elt input-ranges input-index)))
                         (make-range
                          (+ offset (* scaling (range-start input-range)))
                          (* scaling (range-step input-range))
                          (+ offset (* scaling (range-end input-range)))))))))
      (map-transformation-outputs transformation #'store-output-range))
    (shape-from-ranges output-ranges)))

(defmethod broadcasting-transformation ((from-shape shape) (to-shape shape))
  (let* ((from-ranges (ranges from-shape))
         (to-ranges (ranges to-shape))
         (input-rank (length from-ranges))
         (output-rank (length to-ranges))
         (translation (make-array output-rank :initial-element 0))
         (scaling (make-array output-rank :initial-element 1))
         (input-constraints
           (map 'vector (lambda (range)
                          (when (size-one-range-p range)
                            (range-start range)))
                from-ranges)))
    (loop for index below output-rank
          unless (>= index input-rank) do
            (let* ((to-range (elt to-ranges index))
                   (from-range (elt from-ranges index))
                   (to-size (set-size to-range))
                   (from-size (set-size from-range)))
              (cond (;; Select
                     (> to-size from-size)
                     (setf (aref translation index) 0)
                     (setf (aref scaling index) 1))
                    (;; Move
                     (= to-size from-size)
                     (setf (aref translation index)
                           (- (range-start to-range)
                              (range-start from-range)))
                     (setf (aref scaling index)
                           (if (size-one-range-p to-range)
                               0
                               (/ (range-step to-range)
                                  (range-step from-range)))))
                    (;; Broadcast
                     (= 1 to-size)
                     (setf (aref translation index)
                           (- (range-start to-range)
                              (range-start from-range)))
                     (setf (aref scaling index) 0)))))
    (make-transformation
     :input-rank input-rank
     :output-rank output-rank
     :translation translation
     :scaling scaling
     :input-constraints input-constraints)))

(defun collapsing-transformation (shape)
  (invert-transformation
   (from-storage-transformation shape)))

;;; Return a non-permuting, affine transformation from a zero based array
;;; with step size one to the given SHAPE.
(defun from-storage-transformation (shape)
  (let ((ranges (ranges shape)))
    (make-transformation
     :scaling (map 'vector #'range-step ranges)
     :translation (map 'vector #'range-start ranges))))
