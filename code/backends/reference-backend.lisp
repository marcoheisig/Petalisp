;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-reference-backend)

;;; The purpose of the reference backend is to compute reference solutions
;;; for automated testing. It is totally acceptable that this
;;; implementation is slow or eagerly consing, as long as it is obviously
;;; correct.
;;;
;;; Internally, all evaluated arrays are represented as a list of conses of
;;; the form (indices . value), where indices is a list of integers.

(defgeneric evaluate (data-structure backend))

(defclass reference-backend (backend)
  ((%memoization-table :reader memoization-table
                       :initform (make-hash-table :test #'eq)
                       :type hash-table)))

(defmethod compute-immediates :after
    (data-structure (backend reference-backend))
  (clrhash (memoization-table backend)))


(defmethod compute-immediates ((data-structures list) (backend reference-backend))
  (mapcar (lambda (x)
            (canonicalize-data-structure
             (array-from-ir
              (evaluate x backend))))
          data-structures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion between arrays and internal representation

(defun ir-from-array (array)
  (if (null (array-dimensions array))
      (list
       (cons '() (aref array)))
      (loop for indices in (apply #'map-product #'list
                                  (mapcar #'iota (array-dimensions array)))
            collect (cons indices (apply #'aref array indices)))))

(defun array-from-ir (ir &key (element-type t))
  (let* ((array-dimensions
           (mapcar #'1+
                   (if (null ir)
                       ()
                       (reduce (lambda (l1 l2) (mapcar #'max l1 l2)) ir
                               :key #'first))))
         (array
           (make-array array-dimensions :element-type element-type)))
    (loop for (indices . value) in ir do
      (setf (apply #'aref array indices) value))
    array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation

(defun normalize (representation)
  (assert (petalisp::identical representation :key (compose #'length #'first)))
  (sort (copy-list representation)
        (lambda (l1 l2) (every #'<= l1 l2))
        :key #'first))

(defmethod evaluate :around
    ((data-structure data-structure)
     (backend reference-backend))
  (petalisp::with-hash-table-memoization (data-structure)
      (memoization-table backend)
    (normalize (call-next-method))))

(defmethod evaluate ((immediate immediate) (backend reference-backend))
  (ir-from-array (storage-array immediate)))

(defmethod evaluate ((application application) (backend reference-backend))
  (let ((operator (application-operator application)))
    (apply #'mapcar
           (lambda (&rest inputs)
             (assert (petalisp::identical inputs :test #'equal :key #'first))
             (cons (first (first inputs))
                   (apply operator (mapcar #'rest inputs))))
           (mapcar (lambda (x) (evaluate x backend))
                   (inputs application)))))

(defmethod evaluate ((reduction reduction) (backend reference-backend))
  (let ((binop (reduction-binary-operator reduction))
        (unop (reduction-unary-operator reduction))
        (table (make-hash-table :test #'equal)))
    (loop for (indices . value) in (evaluate (input reduction) backend) do
      (multiple-value-bind (accumulator present-p)
          (gethash (rest indices) table)
        (if (not present-p)
            (setf (gethash (first indices) table)
                  (funcall unop value))
            (setf (gethash (first indices) table)
                  (funcall binop accumulator value)))))
    (maphash #'cons table)))

(defmethod evaluate ((fusion fusion) (backend reference-backend))
  (apply #'append (mapcar (lambda (x) (evaluate x backend))
                          (inputs fusion))))

(defmethod evaluate ((reference reference) (backend reference-backend))
  (let ((table (make-hash-table :test #'equal))
        (transformation (transformation reference)))
    (loop for (indices . value) in (evaluate (input reference) backend) do
      (setf (gethash indices table) value))
    (loop for indices in (set-elements (shape reference))
          for input-indices = (funcall transformation indices)
          collect (cons indices
                        (multiple-value-bind (value present-p)
                            (gethash  input-indices table)
                          (assert present-p)
                          value)))))
