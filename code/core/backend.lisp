;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; This special variable will be bound later, once at least one backend
;;; has been loaded.
(defvar *backend*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric compute-on-backend (lazy-arrays backend))

(defgeneric schedule-on-backend (lazy-arrays backend))

(defgeneric compute-immediates (lazy-arrays backend))

(defgeneric lisp-datum-from-immediate (lazy-array))

(defgeneric delete-backend (backend))

(defgeneric backend-devices (backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass backend ()
  ())

(defclass deleted-backend ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-on-backend :before ((lazy-arrays list) (backend t))
  (loop for lazy-array in lazy-arrays do
    (assert (computablep lazy-array))
    ;; Ensure that root nodes are referenced at least twice, such that the
    ;; IR conversion always allocates them in memory.
    (incf (%lazy-array-refcount lazy-array) 2)))

(defmethod schedule-on-backend :before ((lazy-arrays list) (backend t))
  (assert (every #'computablep lazy-arrays)))

(defmethod compute-on-backend ((lazy-arrays list) (backend backend))
  (let* ((collapsing-transformations
           (mapcar (alexandria:compose #'collapsing-transformation #'array-shape)
                   lazy-arrays))
         (immediates
           (compute-immediates
            (mapcar #'transform lazy-arrays collapsing-transformations)
            backend)))
    (loop for lazy-array in lazy-arrays
          for collapsing-transformation in collapsing-transformations
          for immediate in immediates
          do (replace-lazy-array
              lazy-array
              (lazy-reshape immediate (array-shape lazy-array) collapsing-transformation)))
    (values-list
     (mapcar #'lisp-datum-from-immediate immediates))))

(defmethod schedule-on-backend ((lazy-arrays list) (backend backend))
  (compute-on-backend lazy-arrays backend))

(defmethod lisp-datum-from-immediate ((array-immediate array-immediate))
  (if (zerop (rank array-immediate))
      (aref (array-immediate-storage array-immediate))
      (array-immediate-storage array-immediate)))

(defmethod lisp-datum-from-immediate ((range-immediate range-immediate))
  (let* ((shape (array-shape range-immediate))
         (range (first (shape-ranges shape)))
         (size (range-size range))
         (array (make-array size)))
    (loop for index below size do
      (setf (aref array index) index))
    array))

(defmethod delete-backend ((backend backend))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; API

(defun compute (&rest arguments)
  (compute-on-backend
   (mapcar #'lazy-array arguments)
   *backend*))

(defun schedule (&rest arguments)
  (schedule-on-backend
   (mapcar #'lazy-array arguments)
   *backend*))

