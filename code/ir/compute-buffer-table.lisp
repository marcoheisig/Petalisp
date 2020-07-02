;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
;;;
;;; These rules ensure that values that are used more than once reside in
;;; main memory.
;;;
;;; The derivation of the buffer table of a graph consists of two steps.
;;; In the first step, we build a hash table that assigns some strided
;;; arrays the values :POTENTIALLY-SPECIAL or :SPECIAL.  In the second
;;; step, this table turned into a buffer table by replacing :SPECIAL
;;; values by buffers and discarding all other hash table entries.

(defvar *buffer-table*)

(defmacro buffer-table-entry (node)
  `(gethash ,node *buffer-table*))

(defun compute-buffer-table (graph-roots)
  (let ((*buffer-table* (make-hash-table :test #'eq)))
    (loop for graph-root in graph-roots do
      ;; Rule 1
      (traverse-node graph-root t))
    ;; Finalize the buffer table.
    (finalize-buffer-table)
    *buffer-table*))

(defun finalize-buffer-table ()
  (let ((buffer-table (the hash-table *buffer-table*)))
    (maphash
     (lambda (lazy-array value)
       (if (eq value :special)
           (setf (buffer-table-entry lazy-array)
                 (if (typep lazy-array 'range-immediate)
                     '.range-immediate.
                     (make-buffer lazy-array)))
           (remhash lazy-array buffer-table)))
     buffer-table)))

(defun traverse-node (node special-p)
  (multiple-value-bind (traverse-inputs-p inputs-special-p)
      (visit-node node)
    (when special-p
      (setf (buffer-table-entry node) :special))
    (when traverse-inputs-p
      (loop for input in (inputs node) do
        (traverse-node input inputs-special-p)))))

(defgeneric visit-node (node))

(defmethod visit-node ((node lazy-array))
  ;; Rule 2.
  (case (number-of-users node)
    ((0 1) (values t nil))
    (otherwise
     (multiple-value-bind (value present-p) (buffer-table-entry node)
       (cond ((not present-p)
              (setf (buffer-table-entry node) :potentially-special)
              (values t nil))
             ((eq value :potentially-special)
              (setf (buffer-table-entry node) :special)
              (values nil nil))
             ((eq value :special)
              (values nil nil)))))))

(defmethod visit-node ((immediate immediate))
  (setf (buffer-table-entry immediate) :special)
  (values nil nil))

(defmethod visit-node ((lazy-reshape lazy-reshape))
  (multiple-value-bind (traverse-inputs-p inputs-special-p)
      (call-next-method)
    (if (not traverse-inputs-p)
        (values nil nil)
        (let ((transformation (transformation lazy-reshape)))
          (values
           traverse-inputs-p
           ;; Rule 3.
           (or inputs-special-p (not (transformation-invertiblep transformation))))))))
