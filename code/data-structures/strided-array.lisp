;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (data-structure)
  (index-space))

(defmethod dimension ((object strided-array))
  (dimension (index-space object)))

(defmethod size ((object strided-array))
  (size (index-space object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  convert lisp arrays to strided arrays

(define-class strided-array-constant (strided-array constant)
  (data
   (predecessors :initform () :allocation :class)))

(defmethod petalispify ((array array))
  (array->strided-array array))

(defmethod petalisp->lisp ((object data-structure) &optional storage)
  (declare (ignore storage))
  (if (array-dimensions (data object))
      (data object)
      (aref (data object))))

(defmethod print-object ((object strided-array) stream)
  (print-unreadable-object (object stream :type t)
    (princ (index-space object) stream)))

(defun array->strided-array (array)
  (let ((ranges (ranges array)))
    (make-instance
     'strided-array-constant
     :data array
     :element-type (element-type array)
     :ranges ranges)))
