;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (data-structure)
  (ranges index-space))

(defmethod dimension ((object strided-array))
  (length (ranges object)))

(defmethod size ((object strided-array))
  (reduce #'* (ranges object) :key #'size))

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

(defmethod ranges ((array array))
  (map 'vector
       (lambda (end)
         (range 0 1 (1- end)))
       (array-dimensions array)))

(defun array->strided-array (array)
  (let ((ranges (ranges array)))
    (make-instance
     'strided-array-constant
     :data array
     :element-type (element-type array)
     :ranges ranges)))
