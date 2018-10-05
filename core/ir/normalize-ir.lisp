;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;; The IR consists of buffers of arbitrary shape, and of kernels that
;;; reference some buffers via arbitrary affine linear transformations.  A
;;; downside of this representation is that it includes a useless degree of
;;; freedom.  We can reshape each buffer with another affine-linear
;;; transformation, as long as we also update the transformations of all
;;; references to the buffer.
;;;
;;; The purpose of this IR transformation is to get rid of this useless
;;; degree of freedom.  To do so, we reshape each buffer such that all
;;; ranges of its shape have a start of zero and a step size of one.  Of
;;; course, we also update all references to each buffer, such that the
;;; semantics is preserved.

(defun normalize-ir (roots)
  ;; The normalization table is a mapping from buffers to their respective
  ;; normalizing transformation.  A normalizing transformation is a mapping
  ;; from the indices of the old buffer shape to the indices of the new
  ;; buffer shape.
  (map-buffers
   (lambda (buffer)
     (let ((transformation (collapsing-transformation (shape buffer))))
       (unless (identity-transformation-p transformation)
         ;; Update all kernels that store into this buffer.
         (loop for kernel in (inputs buffer) do
           (loop for store in (stores kernel) do
             (when (eq (car store) buffer)
               (setf (cdr store)
                     (compose-transformations transformation (cdr store))))))
         ;; Update all kernels that load from this buffer.
         (loop for kernel in (outputs buffer) do
           (loop for load in (loads kernel) do
             (when (eq (car load) buffer)
               (setf (cdr load)
                     (compose-transformations transformation (cdr load)))))))))
   roots))
