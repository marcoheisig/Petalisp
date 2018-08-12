;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-reference-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-simple-immediate (shape value-fn))

(defgeneric iref (immediate index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass simple-immediate (immediate)
  ((%table :initarg :table :reader table))
  (:default-initargs :element-type t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-simple-immediate ((shape shape) (value-fn function))
  (let ((table (make-hash-table :test #'equal)))
    (loop for index in (set-elements shape) do
      (setf (gethash index table)
            (funcall value-fn index)))
    (make-instance 'simple-immediate
      :shape shape
      :table table)))

(defmethod iref ((simple-immediate simple-immediate) (index list))
  (multiple-value-bind (value present-p)
      (gethash index (table simple-immediate))
    (unless present-p
      (error "Invalid index ~S for the strided array ~S."
             index simple-immediate))
    value))

(defmethod make-strided-array ((simple-immediate simple-immediate))
  (let ((shape (shape simple-immediate))
        (table (table simple-immediate)))
    (if (null (ranges shape))
        (make-instance 'scalar-immediate
          :shape shape
          :storage (gethash '() table))
        (let ((storage (make-array (mapcar #'set-size (ranges shape)))))
          (loop for index in (set-elements shape) do
            (setf (apply #'aref storage index)
                  (gethash index table)))
          (make-instance 'array-immediate
            :shape (shape simple-immediate)
            :storage storage)))))
