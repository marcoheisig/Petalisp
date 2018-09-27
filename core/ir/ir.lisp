;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are strided arrays, into an analogous graph, whose nodes are buffers
;;; and kernels.  Kernels and buffers alternate, such that the inputs and
;;; outputs of a kernel are always buffers, and such that the inputs and
;;; outputs of a buffer are always kernels.
;;;
;;; The IR conversion algorithm proceeds along the following steps:
;;;
;;; 1. A hash table is created that maps certain strided arrays to buffers
;;;    of the same size and element type.  This table is constructed such
;;;    that any subgraph without these nodes is a tree and contains no
;;;    reduction nodes.
;;;
;;; 2. Each root of a subtree from step 1 is turned into one or more
;;;    kernels.  All fusion nodes in the tree are eliminated by choosing
;;;    the iteration space of the kernels appropriately.
;;;
;;; 3. All buffers are updated to contain a list of kernels that read to
;;;    them or write from them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-buffer (strided-array backend))

(defgeneric make-kernel (body backend))

(defgeneric compute-buffer-table (strided-arrays backend))

(defgeneric compute-kernels (root backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-node ()
  ((%inputs :initarg :inputs :accessor inputs)
   (%outputs :initarg :outputs :accessor outputs))
  (:default-initargs :inputs '() :outputs '()))

(defclass buffer (ir-node)
  ((%shape :initarg :shape :reader shape)
   (%element-type :initarg :element-type :reader element-type)))

(defclass kernel (ir-node)
  ((%body :initarg :body :reader body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-buffer ((strided-array strided-array) (backend backend))
  (make-instance 'buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)))

(defmethod make-kernel (body (backend backend))
  (make-instance 'kernel
    :body body))

;;; We compute the inputs and outputs of a kernel during SHARED-INITIALIZE,
;;; such that one can use REINITIALIZE-INSTANCE to recompute them after the
;;; kernel body has changed.
(defmethod shared-initialize :after ((kernel kernel) slots &rest initargs)
  (declare (ignore slots initargs))
  (with-accessors ((inputs inputs)
                   (outputs outputs)) kernel
    (multiple-value-setq (inputs outputs)
      (compute-kernel-inputs-and-outputs (body kernel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The IR conversion entry point

(defvar *buffer-table*)

(defun ir-from-strided-arrays (strided-arrays backend)
  (let ((*buffer-table* (compute-buffer-table strided-arrays backend)))
    ;; Now create a list of kernels for each entry in the buffer table.
    (loop for root being each hash-key of *buffer-table* do
      (let ((kernels (compute-kernels root backend)))
        ;; Update the inputs and outputs of all buffers to match
        ;; the inputs and outputs of the corresponding kernels.
        (loop for kernel in kernels do
          (loop for input in (inputs kernel) do
            (pushnew kernel (outputs input)))
          (loop for output in (outputs kernel) do
            (pushnew kernel (inputs output))))))
    ;; Finally, return the buffers corresponding to the root nodes.
    (loop for strided-array in strided-arrays
          collect (gethash strided-array *buffer-table*))))
