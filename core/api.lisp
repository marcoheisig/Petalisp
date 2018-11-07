;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defvar *backend* (petalisp-native-backend:make-native-backend)
  "The backend on which Petalisp programs are executed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working With Shapes

(defun indices (array &optional (axis 0))
  "Return an array of integers, where the value of each entry (i_0 ... i_N)
is i_AXIS.  If axis is not supplied, it defaults to zero."
  (let* ((strided-array (coerce-to-strided-array array))
         (shape (shape strided-array)))
    (assert (<= 0 axis (1- (dimension shape))))
    (make-reference
     (make-range-immediate (nth axis (ranges shape)))
     shape
     (make-transformation
      :input-dimension (dimension shape)
      :permutation (vector axis)))))

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
             (cond ((or (listp modifier) (integerp modifier))
                    (reshape-with-shape strided-array (make-shape modifier)))
                   ((functionp modifier)
                    (reshape-with-transformation strided-array (make-transformation-from-function modifier)))
                   ((shapep modifier)
                    (reshape-with-shape strided-array modifier))
                   ((transformationp modifier)
                    (reshape-with-transformation strided-array modifier)))))
    (reduce #'reshape1 shapes-and-transformations :initial-value (coerce-to-strided-array array))))

(defun fuse (&rest arrays)
  "Combine ARRAYS into a single strided array. It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion."
  (make-fusion (mapcar #'coerce-to-strided-array arrays)))

(defun fuse* (&rest arrays)
  "Combine ARRAYS into a single strided array. When some of the supplied
arguments overlap partially, the value of the rightmost object is used."
  (let ((strided-arrays (mapcar #'coerce-to-strided-array arrays)))
    (flet ((reference-origin (piece)
             (make-reference
              (find piece strided-arrays :from-end t :key #'shape :test #'set-subsetp)
              piece
              (identity-transformation (dimension piece)))))
      (make-fusion (mapcar #'reference-origin (subdivision (mapcar #'shape strided-arrays)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parallel MAP and REDUCE

(defun broadcast-arguments (arguments)
  (let* ((strided-arrays (mapcar #'coerce-to-strided-array arguments))
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

(defalias a α)

(defalias b β)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation

(defun compute (&rest arguments)
  "Return the computed values of all ARGUMENTS."
  (compute-on-backend
   (mapcar #'coerce-to-strided-array arguments)
   *backend*))

(defun schedule (&rest arguments)
  "Instruct Petalisp to compute all given ARGUMENTS asynchronously."
  (schedule-on-backend
   (mapcar #'coerce-to-strided-array arguments)
   *backend*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp Readtable

(define-constant +whitespace+ '(#\space #\tab #\linefeed #\return #\page)
  :test #'equal)

(defun read-α (stream char)
  (declare (ignore char))
  (if (member (peek-char nil stream) +whitespace+)
      `α
      `(lambda (&rest args)
         (apply 'α #',(read stream) args))))

(defun read-β (stream char)
  (declare (ignore char))
  (if (member (peek-char nil stream) +whitespace+)
      `β
      `(lambda (&rest args)
         (apply 'β #',(read stream) args))))

(named-readtables:defreadtable petalisp-readtable
  (:merge :common-lisp)
  (:macro-char #\α #'read-α)
  (:macro-char #\β #'read-β))
