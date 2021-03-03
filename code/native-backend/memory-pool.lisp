;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric memory-pool-allocate (memory-pool array-element-type array-dimensions))

(defgeneric memory-pool-free (memory-pool array))

(defgeneric memory-pool-reset (memory-pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass memory-pool ()
  ((%array-table :reader array-table
                 :initform (make-hash-table :test #'equalp))))

(defun make-memory-pool ()
  (make-instance 'memory-pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod memory-pool-allocate ((memory-pool memory-pool)
                                 (array-element-type t)
                                 (array-dimensions list))
  (or (pop (gethash (cons array-element-type array-dimensions)
                    (array-table memory-pool)))
      (make-array array-dimensions :element-type array-element-type)))

(defmethod memory-pool-free ((memory-pool memory-pool)
                             (array array))
  (push array (gethash (cons (array-element-type array)
                             (array-dimensions array))
                       (array-table memory-pool)))
  (values))

(defmethod memory-pool-reset ((memory-pool memory-pool))
  (clrhash (array-table memory-pool)))
