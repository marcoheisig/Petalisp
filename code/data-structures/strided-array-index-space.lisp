;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defmethod enlarge-shape ((from shape) (to shape))
  (let ((new-ranges (copy-list (ranges to))))
    (replace new-ranges (ranges from))
    (shape-from-ranges new-ranges)))

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

(defun shape-union-range-oracle (&rest ranges)
  (declare (dynamic-extent ranges))
  ;; determine the bounding box
  (loop for range in ranges
        minimize (range-start range) into global-start
        maximize (range-end range) into global-end
        finally
           (return
             (if (= global-start global-end)
                 (first ranges)
                 ;; now determine the step size
                 (let ((step-size (- global-end global-start)))
                   (dolist (range ranges)
                     (flet ((check (n)
                              (setf step-size
                                    (min step-size
                                         (- n global-start)))))
                       (if (> (range-start range) global-start)
                           (check (range-start range))
                           (unless (size-one-range-p range)
                             (check (+ (range-start range)
                                       (range-step range)))))))
                   (make-range global-start step-size global-end))))))

(defmethod shape-union ((shape-1 shape) &rest more-shapes)
  (shape-from-ranges
   (apply #'mapcar
          #'shape-union-range-oracle
          (ranges shape-1) (mapcar #'ranges more-shapes))))

(defmethod shape-union :around ((shape-1 shape) &rest more-shapes)
  (let ((union (call-next-method)))
    (flet ((proper-subspace-p (shape)
             (subspace-p shape union)))
      (assert (proper-subspace-p shape-1))
      (assert (every #'proper-subspace-p more-shapes))
      union)))

(defmethod transform ((shape shape) (operator identity-transformation))
  shape)

(defmethod transform ((data-structure data-structure) (operator identity-transformation))
  data-structure)
