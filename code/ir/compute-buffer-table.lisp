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
;;; costly, but the alternative of implementing heterogeneous reduction
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

(defgeneric compute-buffer-table (strided-arrays backend))

(defmethod compute-buffer-table ((graph-roots list) (backend backend))
  (let ((*buffer-table* (make-hash-table :test #'eq)))
    (loop for graph-root in graph-roots do
      ;; Rule 1
      (traverse-node graph-root t nil))
    (finalize-buffer-table backend)))

(defun finalize-buffer-table (backend)
  (with-hash-table-iterator (next *buffer-table*)
    (loop
      (multiple-value-bind (more strided-array value) (next)
        (cond ((not more) (return))
              ((eq value :special)
               (setf (node-value strided-array)
                     (if (typep strided-array 'range-immediate)
                         'range-immediate-placeholder
                         (make-buffer strided-array backend))))
              (t (remhash strided-array *buffer-table*))))))
  *buffer-table*)

(defun traverse-node (node special-p reduction-axis)
  (multiple-value-bind (traverse-inputs-p inputs-special-p reduction-axis)
      (visit-node node reduction-axis)
    (when special-p
      (setf (node-value node) :special))
    (when traverse-inputs-p
      (loop for input in (inputs node) do
        (traverse-node input inputs-special-p reduction-axis)))))

(defgeneric visit-node (node reduction-axis))

(defmethod visit-node ((node strided-array) reduction-axis)
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

(defmethod visit-node ((reduction reduction) reduction-axis)
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

(defmethod visit-node ((reference reference) reduction-axis)
  (multiple-value-bind (traverse-inputs-p inputs-special-p reduction-axis)
      (call-next-method)
    (if (not traverse-inputs-p)
        (values nil nil nil)
        (let ((transformation (transformation reference)))
          (values
           traverse-inputs-p
           ;; Rule 3.
           (or inputs-special-p (not (invertible-transformation-p transformation)))
           ;; Adapt the reduction axis.
           (if (null reduction-axis)
               nil
               (transform-axis reduction-axis transformation)))))))

;;; Determine whether a reduction along the given REDUCTION-AXIS would be
;;; heterogeneous, meaning that different parts of the reduction would read
;;; different inputs of the fusion.
(defun breaking-fusion-p (fusion reduction-axis)
  (flet ((reduction-range-of (strided-array)
           (nth reduction-axis (ranges (shape strided-array)))))
    (let ((fusion-range (reduction-range-of fusion)))
      (loop for input in (inputs fusion)
              thereis (not
                       (set-equal
                        fusion-range
                        (reduction-range-of input)))))))

(defmethod visit-node ((fusion fusion) reduction-axis)
  (multiple-value-bind (traverse-inputs-p inputs-special-p reduction-axis)
      (call-next-method)
    (cond ((not traverse-inputs-p)
           (values nil nil nil))
          ;; Rule 6.
          ((breaking-fusion-p fusion reduction-axis)
           (setf (node-value fusion) :special)
           (values t inputs-special-p nil))
          (t
           (values t inputs-special-p reduction-axis)))))
