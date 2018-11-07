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
  (map-buffers
   (lambda (buffer)
     (transform buffer (collapsing-transformation (shape buffer))))
   roots))

(defmethod transform
    ((buffer buffer)
     (transformation transformation))
  (setf (shape buffer)
        (transform (shape buffer) transformation)))

(defmethod transform
    ((instruction instruction)
     (transformation transformation))
  (values))

(defmethod transform
    ((iterating-instruction iterating-instruction)
     (transformation transformation))
  (setf (transformation iterating-instruction)
        (compose-transformations
         transformation
         (transformation iterating-instruction))))

;;; Once a buffer has been transformed, update all loads and stores
;;; referencing the buffer to preserve the semantics of the IR.
(defmethod transform :after ((buffer buffer) (transformation transformation))
  (loop for kernel in (inputs buffer) do
    (loop for store in (stores kernel) do
      (when (eq (buffer store) buffer)
        (transform store transformation))))
  (loop for kernel in (outputs buffer) do
    (loop for load in (loads kernel) do
      (when (eq (buffer load) buffer)
        (transform load transformation)))))
