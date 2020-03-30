;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The buffer table is a hash table that maps certain strided arrays of a
;;; data flow graph to their corresponding buffers.  The following criteria
;;; are used to select the strided arrays/graph nodes that are registered
;;; in the buffer table:
;;;
;;; 1. The node is one of the root nodes.
;;; 2. The node is referenced by multiple other nodes.
;;; 3. The node is the target of a broadcasting reference.
;;; 4. The node is an immediate node.
;;; 5. The node is a reduction node.
;;; 6. The node is a fusion node and would break a reduction range.
;;;
;;; These rules 1-4 ensure that values that are used more than once reside
;;; in main memory.  Rule 5 is chosen in addition, because it greatly
;;; simplifies reasoning about kernels, while the cost of allocating the
;;; results of a reduction is usually negligible.  Rule 6 is potentially
;;; costly, but the alternative of dealing with heterogeneous reduction
;;; kernels is a nightmare.
;;;
;;; The derivation of the buffer table of a graph consists of two steps.
;;; In the first step, we build a hash table that assigns some strided
;;; arrays the values :POTENTIALLY-SPECIAL or :SPECIAL.  In the second
;;; step, this table turned into a buffer table by replacing :SPECIAL
;;; values by buffers and discarding all other hash table entries.

(defvar *buffer-table*)

(defmacro node-value (node)
  `(gethash ,node *buffer-table*))

(defun compute-buffer-table (graph-roots)
  (let ((*buffer-table* (make-hash-table :test #'eq)))
    (loop for graph-root in graph-roots do
      ;; Rule 1
      (traverse-node graph-root t nil))
    ;; Finalize the buffer table.
    (finalize-buffer-table)
    *buffer-table*))

(defun finalize-buffer-table ()
  (let ((buffer-table (the hash-table *buffer-table*)))
    (maphash
     (lambda (lazy-array value)
       (if (eq value :special)
           (setf (node-value lazy-array)
                 (if (typep lazy-array 'range-immediate)
                     '.range-immediate.
                     (make-buffer lazy-array)))
           (remhash lazy-array buffer-table)))
     buffer-table)))

(defun traverse-node (node special-p reduction-axis)
  (multiple-value-bind (traverse-inputs-p inputs-special-p reduction-axis)
      (visit-node node reduction-axis)
    (when special-p
      (setf (node-value node) :special))
    (when traverse-inputs-p
      (loop for input in (inputs node) do
        (traverse-node input inputs-special-p reduction-axis)))))

(defgeneric visit-node (node reduction-axis))

(defmethod visit-node ((node lazy-array) reduction-axis)
  (case (refcount node)
    ((0 1) (values t nil reduction-axis))
    (otherwise
     ;; Rule 2.
     (multiple-value-bind (value present-p) (node-value node)
       (cond ((not present-p)
              (setf (node-value node) :potentially-special)
              (values t nil reduction-axis))
             ((eq value :potentially-special)
              (setf (node-value node) :special)
              (values nil nil nil))
             ((eq value :special)
              (values nil nil nil)))))))

(defmethod visit-node ((reduction lazy-reduce) reduction-axis)
  (multiple-value-bind (value present-p) (node-value reduction)
    (cond ((not present-p)
           (unless (eq value :special)
             (setf (node-value reduction) :special))
           (values t nil 0))
          (t
           (values nil nil nil)))))

(defmethod visit-node ((immediate immediate) reduction-axis)
  (setf (node-value immediate) :special)
  (values nil nil nil))

(defmethod visit-node ((lazy-reshape lazy-reshape) reduction-axis)
  (multiple-value-bind (traverse-inputs-p inputs-special-p reduction-axis)
      (call-next-method)
    (if (not traverse-inputs-p)
        (values nil nil nil)
        (let ((transformation (transformation lazy-reshape)))
          (values
           traverse-inputs-p
           ;; Rule 3.
           (or inputs-special-p (not (transformation-invertiblep transformation)))
           ;; Adapt the reduction axis.
           (if (null reduction-axis)
               nil
               (transform-axis reduction-axis transformation)))))))

(defmethod visit-node ((lazy-fuse lazy-fuse) reduction-axis)
  (multiple-value-bind (traverse-inputs-p inputs-special-p reduction-axis)
      (call-next-method)
    (cond ((not traverse-inputs-p)
           (values nil nil nil))
          ;; Rule 6.
          ((breaking-fusion-p lazy-fuse reduction-axis)
           (setf (node-value lazy-fuse) :special)
           (values t inputs-special-p nil))
          (t
           (values t inputs-special-p reduction-axis)))))

;;; Determine whether a reduction along the given REDUCTION-AXIS would be
;;; heterogeneous, meaning that different parts of the reduction would read
;;; different inputs of the fusion.
(defun breaking-fusion-p (fusion reduction-axis)
  (if (null reduction-axis)
      nil
      (flet ((reduction-range-of (lazy-array)
               (nth reduction-axis (shape-ranges (shape lazy-array)))))
        (let ((fusion-range (reduction-range-of fusion)))
          (loop for input in (inputs fusion)
                  thereis (not
                           (range-equal
                            fusion-range
                            (reduction-range-of input))))))))
