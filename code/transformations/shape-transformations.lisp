;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defmethod transform :before ((shape shape) (transformation transformation))
  (demand (= (dimension shape) (input-dimension transformation))
    "~@<Cannot apply the transformation ~A with input dimension ~R ~
        to the index shape ~A with dimension ~R.~:@>"
    transformation (input-dimension transformation)
    shape (dimension shape)))

(defmethod transform ((shape shape) (operator identity-transformation))
  shape)

(defmethod transform :before ((shape shape) (transformation transformation))
  (when-let ((input-constraints (input-constraints transformation)))
    (loop for range in (ranges shape)
          for constraint across input-constraints
          for index from 0 do
            (unless (not constraint)
              (demand (and (= constraint (range-start range))
                           (= constraint (range-end range)))
                "~@<The ~:R dimension of the shape ~W violates ~
                    the input constraint ~W of the transformation ~W.~:@>"
                index shape constraint transformation)))))

(defmethod transform ((shape shape)
                      (transformation hairy-transformation))
  (let ((output-ranges (make-list (output-dimension transformation)))
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
