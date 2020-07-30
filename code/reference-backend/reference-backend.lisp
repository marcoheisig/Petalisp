;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.reference-backend)

;;; The purpose of the reference backend is to compute reference solutions
;;; for automated testing. It is totally acceptable that this
;;; implementation is slow or eagerly consing, as long as it is obviously
;;; correct.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric evaluate (lazy-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reference-backend (backend)
  ())

(defun make-reference-backend ()
  (make-instance 'reference-backend
    :machine (host-machine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-immediates ((lazy-arrays list) (backend reference-backend))
  (mapcar #'evaluate lazy-arrays))

;;; Memoization

(defvar *memoization-table*)

(defmethod compute-immediates :around
    ((lazy-arrays list) (backend reference-backend))
  (let ((*memoization-table* (make-hash-table :test #'eq)))
    (call-next-method)))

(defmethod evaluate :around ((lazy-array lazy-array))
  (petalisp.utilities:with-hash-table-memoization (lazy-array)
      *memoization-table*
    (call-next-method)))

;;; Evaluation

(defmethod evaluate :before ((lazy-array lazy-array))
  (assert (every #'lazy-array-p (inputs lazy-array))))

(defmethod evaluate ((simple-immediate simple-immediate))
  simple-immediate)

(defmethod evaluate ((array-immediate array-immediate))
  (make-simple-immediate
   (shape array-immediate)
   (element-type array-immediate)
   (lambda (index)
     (apply #'aref (storage array-immediate) index))))

(defmethod evaluate ((range-immediate range-immediate))
  (make-simple-immediate
   (shape range-immediate)
   (element-type range-immediate)
   #'first))

(defmethod evaluate ((lazy-map single-value-lazy-map))
  (let ((inputs (mapcar #'evaluate (inputs lazy-map))))
    (make-simple-immediate
     (shape lazy-map)
     (element-type lazy-map)
     (lambda (index)
       (values
        (apply (operator lazy-map)
               (mapcar (lambda (input) (iref input index)) inputs)))))))

(defmethod evaluate ((lazy-map multiple-value-lazy-map))
  (let ((inputs (mapcar #'evaluate (inputs lazy-map))))
    (make-simple-immediate
     (shape lazy-map)
     (element-type lazy-map)
     (lambda (index)
       (nth
        (value-n lazy-map)
        (multiple-value-list
         (apply (operator lazy-map)
                (mapcar (lambda (input) (iref input index)) inputs))))))))

(defmethod evaluate ((lazy-fuse lazy-fuse))
  (let ((inputs (mapcar #'evaluate (inputs lazy-fuse))))
    (make-simple-immediate
     (shape lazy-fuse)
     (element-type lazy-fuse)
     (lambda (index)
       (let ((input (find-if (lambda (input) (shape-contains (shape input) index)) inputs)))
         (assert input)
         (iref input index))))))

(defmethod evaluate ((lazy-reshape lazy-reshape))
  (let ((input (evaluate (input lazy-reshape))))
    (make-simple-immediate
     (shape lazy-reshape)
     (element-type lazy-reshape)
     (lambda (index)
       (iref input (transform index (transformation lazy-reshape)))))))
