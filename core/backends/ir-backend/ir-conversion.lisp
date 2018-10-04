;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

(defmethod petalisp-ir:make-buffer
    ((strided-array strided-array) (ir-backend ir-backend))
  (make-instance 'ir-backend-buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)
    :storage (make-array (mapcar #'set-size (ranges (shape strided-array))))
    :executedp nil))

(defmethod petalisp-ir:make-buffer
    ((array-immediate array-immediate) (ir-backend ir-backend))
  (make-instance 'ir-backend-buffer
    :shape (shape array-immediate)
    :element-type (element-type array-immediate)
    :storage (storage array-immediate)
    :executedp t))

(defmethod petalisp-ir:make-buffer
    ((scalar-immediate scalar-immediate) (ir-backend ir-backend))
  (make-instance 'ir-backend-buffer
    :shape (shape scalar-immediate)
    :element-type (element-type scalar-immediate)
    :storage (make-array '() :initial-element (storage scalar-immediate))
    :executedp t))

(defmethod petalisp-ir:make-buffer
    ((range-immediate range-immediate) (ir-backend ir-backend))
  (let ((transformation (collapsing-transformation (shape range-immediate)))
        (axis (axis range-immediate)))
    (make-instance 'ir-backend-buffer
      :shape (shape range-immediate)
      :element-type (element-type range-immediate)
      :storage
      (let ((storage (make-array (mapcar #'set-size (ranges (shape range-immediate))))))
        (loop for index in (set-elements (shape range-immediate)) do
          (setf (apply #'aref storage (transform index transformation))
                (nth axis index)))
        storage)
      :executedp t)))
