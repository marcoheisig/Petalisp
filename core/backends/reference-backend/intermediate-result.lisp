;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-reference-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass intermediate-result ()
  ((%shape :initarg :shape :reader shape)
   (%table :initarg :table :reader table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions

(defun make-intermediate-result (shape value-fn)
  (let ((table (make-hash-table :test #'equal)))
    (loop for index in (set-elements shape) do
      (setf (gethash index table)
            (funcall value-fn index)))
    (make-instance 'intermediate-result
      :shape shape
      :table table)))

(defun iref (intermediate-result index)
  (multiple-value-bind (value present-p)
      (gethash index (table intermediate-result))
    (unless present-p
      (error "Invalid index ~S for the strided array ~S."
             index intermediate-result))
    value))

(defun immediate-from-intermediate-result (intermediate-result)
  (let ((shape (shape intermediate-result))
        (table (table intermediate-result)))
    (if (zerop (dimension shape))
        (make-instance 'scalar-immediate
          :shape shape
          :storage (gethash '() table))
        (let ((storage (make-array (mapcar #'set-size (ranges shape)))))
          (loop for index in (set-elements shape) do
            (setf (apply #'aref storage index)
                  (gethash index table)))
          (make-instance 'array-immediate
            :shape (shape intermediate-result)
            :storage storage)))))
