;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-
;;;;
;;;; The allocation table provides O(1) lookup and deletion of entries
;;;; whose keys are pairs of an ntype and a shape.

(in-package #:petalisp.multicore-backend)

(defstruct (allocation-table
            (:copier nil)
            (:constructor make-allocation-table))
  ;; A vector of shape tables, one for each ntype.
  (shape-tables
   (let ((vector (make-array (length petalisp.type-inference:*ntypes*))))
     (loop for index below (length vector) do
       (setf (svref vector index)
             (make-shape-table)))
     vector)
   :type simple-vector))

(defun allocation-table-value (allocation-table shape ntype)
  (declare (allocation-table allocation-table)
           (shape shape)
           (petalisp.type-inference:ntype ntype))
  (shape-table-value
   (svref (allocation-table-shape-tables allocation-table)
          (petalisp.type-inference:ntype-id ntype))
   shape))

(defun (setf allocation-table-value) (value allocation-table shape ntype)
  (setf (shape-table-value
         (svref (allocation-table-shape-tables allocation-table)
                (petalisp.type-inference:ntype-id ntype))
         shape)
        value))

(defun clear-allocation-table (allocation-table)
  (loop for shape-table across (allocation-table-shape-tables allocation-table) do
    (clear-shape-table shape-table)))
