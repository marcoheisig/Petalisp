;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod transform :before ((shape shape) (transformation transformation))
  (assert (= (rank shape) (input-rank transformation)) ()
          "~@<Cannot apply the transformation ~A with input rank ~R ~
              to the index shape ~A with rank ~R.~:@>"
    transformation (input-rank transformation)
    shape (rank shape)))

(defmethod transform :before ((shape shape) (transformation hairy-transformation))
  (let ((input-mask (input-mask transformation)))
    (loop for range in (ranges shape)
          for constraint across input-mask
          for index from 0 do
            (unless (not constraint)
              (assert (and (= constraint (range-start range))
                           (= constraint (range-end range)))
                      ()
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
      (map-transformation-outputs #'store-output-range transformation))
    (apply #'make-shape output-ranges)))

(defun collapsing-transformation (shape)
  (invert-transformation
   (from-storage-transformation shape)))

;;; Return a non-permuting, affine transformation from a zero based array
;;; with step size one to the given SHAPE.
(defun from-storage-transformation (shape)
  (let ((ranges (ranges shape)))
    (make-transformation
     :scalings (map 'vector #'range-step ranges)
     :offsets (map 'vector #'range-start ranges))))
