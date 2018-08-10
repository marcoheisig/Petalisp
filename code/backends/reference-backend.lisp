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

(defgeneric convert-to-immediate (data-structure backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reference-backend (backend)
  ((%memoization-table :reader memoization-table
                       :initform (make-hash-table :test #'eq)
                       :type hash-table)))

(defclass simple-immediate (immediate)
  ((%table :initarg :table :reader table))
  (:default-initargs :element-type t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-immediates :after
    (data-structure (backend reference-backend))
  (clrhash (memoization-table backend)))


(defmethod compute-immediates ((data-structures list) (backend reference-backend))
  (loop for data-structure in data-structures
        collect (convert-to-immediate (evaluate data-structure backend) backend)))

(defmethod convert-to-immediate ((simple-immediate simple-immediate)
                                 (backend reference-backend))
  (let ((storage (make-array (map 'list #'set-size (ranges (shape simple-immediate))))))
    (loop for indices in (set-elements (shape simple-immediate)) do
      (setf (apply #'aref storage indices)
            (gethash-or-die indices (table simple-immediate))))
    (canonicalize-data-structure storage)))

(defmethod evaluate :around
    ((data-structure data-structure)
     (backend reference-backend))
  (petalisp-memoization:with-hash-table-memoization (data-structure)
      (memoization-table backend)
    (let ((table (call-next-method)))
      (assert (= (hash-table-count table)
                 (set-size (shape data-structure))))
      (make-instance 'simple-immediate
        :shape (shape data-structure)
        :element-type (element-type data-structure)
        :table table))))

(defmethod evaluate ((strided-array-immediate strided-array-immediate)
                     (backend reference-backend))
  (let ((storage (storage-array strided-array-immediate))
        (shape (shape strided-array-immediate))
        (table (make-hash-table :test #'equal)))
    (loop for indices in (set-elements shape) do
      (setf (gethash indices table)
            (apply #'aref storage indices)))
    table))

(defmethod evaluate ((application application) (backend reference-backend))
  (let ((input-tables
          (loop for input in (inputs application)
                collect (table (evaluate input backend))))
        (table
          (make-hash-table :test #'equal)))
    (loop for indices being the hash-keys of (first input-tables) do
      (setf (gethash indices table)
            (apply (application-operator application)
                   (mapcar (lambda (input-table) (gethash-or-die indices input-table))
                           input-tables))))
    table))

(defmethod evaluate ((reduction reduction) (backend reference-backend))
  (let ((binop (reduction-binary-operator reduction))
        (unop (reduction-unary-operator reduction))
        (predicate (ecase (reduction-order reduction)
                     (:up #'<)
                     (:down #'>)
                     (:arbitrary #'<)))
        (table (make-hash-table :test #'equal)))
    ;; Build a hash table of alists.
    (maphash
     (lambda (indices value)
       (push (cons (car indices) value)
             (gethash (cdr indices) table)))
     (table (evaluate (input reduction) backend)))
    ;; Sort and evaluate each alist in-place.
    (maphash
     (lambda (indices value)
       (setf (gethash indices table)
             (let ((alist (sort value predicate :key #'car)))
               (reduce binop (cdr alist)
                       :key #'cdr
                       :initial-value (funcall unop (cdar alist))))))
     table)
    table))

(defmethod evaluate ((fusion fusion) (backend reference-backend))
  (alist-hash-table
   (loop for input in (inputs fusion)
         append (hash-table-alist
                 (table
                  (evaluate input backend))))
   :test #'equal))

(defmethod evaluate ((reference reference) (backend reference-backend))
  (let ((transformation (transformation reference))
        (input-table (table (evaluate (input reference) backend)))
        (table (make-hash-table :test #'equal)))
    (loop for indices in (set-elements (shape reference)) do
      (setf (gethash indices table)
            (gethash-or-die (transform indices transformation) input-table)))
    table))

(defun gethash-or-die (key hash-table)
  (multiple-value-bind (value present-p)
      (gethash key hash-table)
    (assert present-p)
    value))
