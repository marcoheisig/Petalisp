;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; The buffer table is a hash table that maps certain strided arrays of a
;;; data flow graph to their corresponding buffers.  The following criteria
;;; are used to select the strided arrays/graph nodes that are registered
;;; in the buffer table:
;;;
;;; 1. The node is one of the root nodes.
;;; 2. The node is referenced by multiple other nodes.
;;; 3. The node is the target of a broadcasting reference.
;;; 4. The node is a reduction node.
;;;
;;; These rules 1-3 ensure that values that are used more than once reside
;;; in main memory.  Rule 4 is not chosen in addition, because it greatly
;;; simplifies reasoning about kernels, while the cost of allocating the
;;; results of a reduction is usually negligible.
;;;
;;; The derivation of the buffer table of a graph consists of two steps.
;;; In the first step, we build a hash table that assigns each strided
;;; array that should have a corresponding buffer a number bigger than one.
;;; In the second step, this table turned into a buffer table by replacing
;;; values bigger than one by buffers and discarding all other hash table
;;; entries.

(defun make-buffer-table (graph-roots backend)
  (let ((refcount-table (make-hash-table :test #'eq)))
    (loop for graph-root in graph-roots do
      ;; Rule 1
      (traverse-special-node graph-root refcount-table))
    (buffer-table-from-refcount-table refcount-table backend)))

(defun buffer-table-from-refcount-table (hash-table backend)
  (with-hash-table-iterator (next hash-table)
    (loop
      (multiple-value-bind (more strided-array refcount) (next)
        (cond ((not more)
               (return))
              ((< 1 refcount)
               (setf (gethash strided-array hash-table)
                     (make-buffer strided-array backend)))
              (t
               (remhash strided-array hash-table))))))
  hash-table)

(defun traverse-special-node (node hash-table)
  (when (not (nth-value 1 (gethash node hash-table)))
    (incf (gethash node hash-table 0) 2)
    (traverse-node-inputs node hash-table)))

(defun traverse-node (node hash-table)
  (cond
    ;; Rule 4
    ((typep node 'reduction)
     (traverse-special-node node hash-table))
    ((>= 1 (refcount node))
     (traverse-node-inputs node hash-table))
    (t
     ;; Rule 2
     (when (= 1 (incf (gethash node hash-table 0)))
       (traverse-node-inputs node hash-table)))))

(defun traverse-node-inputs (node hash-table)
  (if (and (typep node 'reference)
           (not (invertible-transformation-p (transformation node))))
      ;; Rule 3
      (traverse-special-node (input node) hash-table)
      (loop for input in (inputs node) do
        (traverse-node input hash-table))))

