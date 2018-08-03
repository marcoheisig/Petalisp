;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special Variables

(defparameter *backend*
  (make-instance 'reference-backend)
  "The backend on which Petalisp programs are executed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Combination and Reordering

(defmacro with-index-space-accessors ((rank start step end) datum &body body)
  "For an index space specified by DATUM, bind rank to the rank of the
specified index space. Furthermore, bind start, step and end to functions
that map indices in the range from zero below the rank to the respective
quantities."
  (check-type rank symbol)
  (check-type start symbol)
  (check-type step symbol)
  (check-type end symbol)
  (with-gensyms (ranges)
    `(let* ((,ranges (ranges (canonicalize-index-space ,datum)))
            (,rank (length ,ranges)))
       (flet ((,start (index) (range-start (svref ,ranges index)))
              (,step (index) (range-step (svref ,ranges index)))
              (,end (index) (range-end (svref ,ranges index))))
         ,@body))))

(defun reshape (data shape)
  "Return a data structure of given SHAPE, either by selecting a subset of
the elements of DATA, or by broadcasting them.

Examples:
 (reshape 0 '(10 10))          ; Create a 10x10 array of zeros
 (reshape #(1 2 3 4) '((1 2))) ; Select the two interior entries"
  (let* ((data (canonicalize-data-structure data))
         (space (canonicalize-index-space shape)))
    (broadcast data space)))

#+nil
(defun transform (data-structure transformation)
  "Reorder the index-value entries of DATA-STRUCTURE by applying
TRANSFORMATION to each index.

Examples:
 (transform A (τ (i j) (j i))) ; Transpose a matrix

 (defun flip (&rest indices) (mapcar #'- indices))
 (transform A #'flip) ; Flip the sign of each index"
  (let* ((data-structure (canonicalize-data-structure data-structure))
         (transformation (canonicalize-transformation transformation))
         (space (transform (index-space data-structure) transformation))
         (transformation (invert-transformation transformation)))
    (make-reference data-structure space transformation)))

(defun fuse (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. It is an error if
some of the inputs overlap, or if there exists no suitable data structure
to represent the fusion."
  (let ((immediates (mapcar #'canonicalize-data-structure objects)))
    (make-fusion (first immediates) immediates)))

(defun fuse* (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. When some OBJECTS
overlap partially, the value of the rightmost object is used."
  (declare (optimize (debug 3)))
  (let ((objects (mapcar #'canonicalize-data-structure objects)))
    (flet ((reference-origin (piece)
             (make-reference
              (find piece objects :from-end t :key #'index-space :test #'subspace-p)
              piece
              (make-identity-transformation (dimension piece)))))
      (let ((inputs
              (mapcar #'reference-origin
                      (subdivision (mapcar #'index-space objects)))))
        (make-fusion (first inputs) inputs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parallel MAP and REDUCE

(defun α (function object &rest more-objects)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcast."
  (let* ((objects (cons (canonicalize-data-structure object)
                        (mapcar #'canonicalize-data-structure more-objects)))
         (space (apply #'common-broadcast-space (mapcar #'index-space objects)))
         (inputs (mapcar (lambda (object) (broadcast object space)) objects)))
    (make-application function (first inputs) inputs)))

(defun β (f &rest args)
  "Reduce the last dimension of OBJECT with F, using G to convert single
values to the appropriate result type."
  (trivia:ematch args
    ((list g object)
     (make-reduction f g (canonicalize-data-structure object) :up))
    ((list object)
     (make-reduction f #'identity (canonicalize-data-structure object) :up))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation

(defun compute (&rest data-structures)
  "Return the computed values of all OBJECTS."
  (values-list
   (mapcar #'storage-array
           (lparallel.promise:force
            (apply #'schedule data-structures)))))

(defun schedule (&rest data-structures)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (compute-synchronously
   (mapcar #'canonicalize-data-structure data-structures)
   *backend*))
