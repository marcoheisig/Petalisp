;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

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

(defgeneric make-kernel (iteration-space blueprint backend))

(defgeneric make-buffer-table (strided-arrays backend))

(defgeneric compute-kernels (root backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-node ()
  ((%shape :initarg :shape :reader shape)
   (%inputs :initarg :inputs :accessor inputs)
   (%outputs :initarg :outputs :accessor outputs))
  (:default-initargs :inputs '() :outputs '()))

(defclass buffer (ir-node)
  ((%element-type :initarg :element-type :reader element-type)))

(defclass array-buffer (buffer)
  (;; A mapping from indices of the array's shape to storage indices.
   (%transformation :initarg :transformation :accessor transformation)
   ;; The storage of the buffer.  Initially, the storage of a buffer is
   ;; NIL.  Before kernels can write to this buffer, its storage slot must
   ;; be set to an array of the appropriate size.
   (%storage :initarg :storage :initform nil :accessor storage)))

(defclass kernel (ir-node)
  ((%body :initarg :body :reader body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-buffer ((strided-array strided-array) (backend backend))
  (make-instance 'buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)))

(defmethod initialize-instance ((array-buffer array-buffer) &key &allow-other-keys)
  (prog1 (call-next-method)
    (setf (transformation array-buffer)
          (collapsing-transformation (shape array-buffer)))))

(defmethod make-kernel ((iteration-space shape)
                        (body list)
                        (backend backend))
  (multiple-value-bind (inputs outputs)
      (kernel-body-inputs-and-outputs body)
    (make-instance 'kernel
      :shape iteration-space
      :inputs inputs
      :outputs outputs
      :body body)))

(defun kernel-body-inputs-and-outputs (body)
  (let ((inputs '())
        (outputs '()))
    (labels ((scan (form)
               (trivia:ematch form
                 ((list 'pset output form)
                  (push output outputs)
                  (scan form))
                 ((list 'pref input _)
                  (push input inputs))
                 ((or (list* 'preduce _ forms)
                      (list* 'papply _ forms))
                  (mapc #'scan forms)))
               (values)))
      (scan body))
    (values inputs outputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The IR conversion entry point

(defvar *buffer-table*)

(defun ir-from-strided-arrays (strided-arrays backend)
  (let ((*buffer-table* (make-buffer-table strided-arrays backend)))
    ;; Now create a list of kernels for each entry in the buffer table.
    (loop for root being each hash-key of *buffer-table*
            using (hash-value buffer) do
              (let ((kernels (compute-kernels root backend)))
                ;; Update the inputs and outputs of all buffers to match
                ;; the inputs and outputs of the corresponding kernels.
                (loop for kernel in kernels do
                  (loop for input in (inputs kernel) do
                    (push kernel (outputs input)))
                  (loop for output in (outputs kernel) do
                    (push kernel (inputs output))))))
    ;; Finally, return the buffers corresponding to the root nodes.
    (loop for strided-array in strided-arrays
          collect (gethash strided-array *buffer-table*))))
