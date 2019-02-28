;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; The class explicit set provides a straightforward implementation of the
;;; entire set protocol.  The class can serve both as a reference
;;; implementation, and as a fallback solution for finite sets that do not
;;; provide certain operations.

(defclass explicit-set (finite-set)
  ((%table :initarg :table :reader set-element-table)))

(defun make-explicit-set (sequence)
  (if (alexandria:emptyp sequence)
      (empty-set)
      (let ((table (make-hash-table :test #'equal)))
        (flet ((insert (element)
                 (setf (gethash element table) t)))
          (map nil #'insert sequence))
        (make-instance 'explicit-set :table table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on Explicit Sets

(defmethod set-for-each ((function function) (set explicit-set))
  (loop for element being the hash-keys of (set-element-table set) do
    (funcall function element)))

(defmethod set-size ((set explicit-set))
  (hash-table-count (set-element-table set)))

(defmethod set-contains ((set explicit-set) (object t))
  (values (gethash object (set-element-table set))))

(defmethod set-difference ((set-1 explicit-set) (set-2 explicit-set))
  (let ((table (copy-hash-table (set-element-table set-1))))
    (loop for element being the hash-keys of (set-element-table set-2) do
      (remhash element table))
    (make-instance 'explicit-set :table table)))

(defmethod set-equal ((set-1 explicit-set) (set-2 explicit-set))
  (let ((table-1 (set-element-table set-1))
        (table-2 (set-element-table set-2)))
    (and (= (hash-table-count table-1)
            (hash-table-count table-2))
         (loop for element being the hash-keys of table-1
               always (gethash element table-2)))))

(defmethod set-intersection ((set-1 explicit-set) (set-2 explicit-set))
  (let ((table-1 (set-element-table set-1))
        (table-2 (set-element-table set-2))
        (result-table (make-hash-table :test #'equal)))
    (loop for element being the hash-keys of table-1
          when (gethash element table-2) do
            (setf (gethash element result-table) t))
    (if (zerop (hash-table-count result-table))
        (empty-set)
        (make-instance 'explicit-set :table result-table))))

(defmethod set-intersectionp ((set-1 explicit-set) (set-2 explicit-set))
  (let ((table-1 (set-element-table set-1))
        (table-2 (set-element-table set-2)))
    (loop for element being the hash-keys of table-1
            thereis (gethash element table-2))))

(defmethod set-union ((set-1 explicit-set) (set-2 explicit-set))
  (let ((table (copy-hash-table (set-element-table set-1))))
    (loop for element being the hash-keys of (set-element-table set-2) do
      (setf (gethash element table) t))
    (make-instance 'explicit-set :table table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods Producing Explicit Sets

(defmethod set-difference ((set-1 finite-set) (set-2 finite-set))
  (make-explicit-set
   (cl:set-difference (set-elements set-1) (set-elements set-2) :test #'equal)))

(defmethod set-intersection ((set-1 finite-set) (set-2 finite-set))
  (make-explicit-set
   (intersection (set-elements set-1) (set-elements set-2) :test #'equal)))
