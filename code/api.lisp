;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special Variables

(defvar *backend* (make-instance 'reference-backend)
  "The backend on which Petalisp programs are executed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working With Shapes

(defmacro with-shape-accessors ((rank start step end) datum &body body)
  "For a shape specified by DATUM, bind rank to the rank of the specified
shape. Furthermore, bind start, step and end to functions that map
indices in the range from zero below the rank to the respective
quantities."
  (check-type rank symbol)
  (check-type start symbol)
  (check-type step symbol)
  (check-type end symbol)
  (with-gensyms (ranges)
    `(let* ((,ranges (ranges (shape (strided-array ,datum))))
            (,rank (length ,ranges)))
       (flet ((,start (index) (range-start (elt ,ranges index)))
              (,step (index) (range-step (elt ,ranges index)))
              (,end (index) (range-end (elt ,ranges index))))
         ,@body))))

(defun indices (array &optional (axis nil axisp))
  "Return a list, containing one array of integers for each dimension of
ARRAY.  Each returned array contains the index values varying along the
corresponding axis.

If the optional argument AXIS is given, return only the one index array
corresponding to that axis."
  (let ((strided-array (strided-array array)))
    (if axisp
        (make-range-immediate (shape strided-array) axis)
        (loop for axis below (dimension strided-array)
              collect (make-range-immediate (shape strided-array) axis)))))

(defun reshape (array &rest shapes-and-transformations)
  "Return a data structure of given SHAPE, either by selecting a subset of
the elements of DATA, or by broadcasting them.

Examples:
 (reshape 0 '(10 10))          ; Create a 10x10 array of zeros
 (reshape #(1 2 3 4) '((1 2))) ; Select the two interior entries"
  (labels ((reshape-with-shape (strided-array shape)
             (make-reference
              strided-array
              shape
              (broadcasting-transformation shape (shape strided-array))))
           (reshape-with-transformation (strided-array transformation)
             (make-reference
              strided-array
              (transform (shape strided-array) transformation)
              (invert-transformation transformation)))
           (reshape1 (strided-array modifier)
             (cond ((listp modifier)
                    (reshape-with-shape strided-array (make-shape modifier)))
                   ((functionp modifier)
                    (reshape-with-transformation strided-array (make-transformation-from-function modifier)))
                   ((shapep modifier)
                    (reshape-with-shape strided-array modifier))
                   ((transformationp modifier)
                    (reshape-with-transformation strided-array modifier)))))
    (reduce #'reshape1 shapes-and-transformations :initial-value (strided-array array))))

(defun fuse (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. It is an error if
some of the inputs overlap, or if there exists no suitable data structure
to represent the fusion."
  (make-fusion (mapcar #'strided-array objects)))

(defun fuse* (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. When some OBJECTS
overlap partially, the value of the rightmost object is used."
  (let ((objects (mapcar #'strided-array objects)))
    (flet ((reference-origin (piece)
             (make-reference
              (find piece objects :from-end t :key #'shape :test #'set-subsetp)
              piece
              (make-identity-transformation (dimension piece)))))
      (make-fusion (mapcar #'reference-origin (subdivision (mapcar #'shape objects)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parallel MAP and REDUCE

(defun broadcast-arguments (arguments)
  (let* ((strided-arrays (mapcar #'strided-array arguments))
         (shape (broadcast-shapes (mapcar #'shape strided-arrays))))
    (mapcar (lambda (strided-array) (reshape strided-array shape))
            strided-arrays)))

(defun α (function array &rest more-arrays)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcast."
  (make-application function (broadcast-arguments (list* array more-arrays))))

(defun β (function array &rest more-arrays)
  (make-reduction function (broadcast-arguments (list* array more-arrays))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation

(defun compute (&rest data-structures)
  "Return the computed values of all OBJECTS."
  (compute-on-backend
   (mapcar #'strided-array data-structures)
   *backend*))

(defun schedule (&rest data-structures)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (schedule-on-backend
   (mapcar #'strided-array data-structures)
   *backend*))
