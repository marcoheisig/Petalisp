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

(defgeneric compute-buffer-table (strided-arrays backend))

(defgeneric compute-kernels (root backend))

(defgeneric make-buffer (strided-array backend))

(defgeneric make-statement
    (backend &key operator loads stores))

(defgeneric make-simple-kernel
    (backend &key iteration-space body))

(defgeneric make-reduction-kernel
    (backend &key reduction-range iteration-space operator reduction-stores body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass buffer ()
  ((%shape :initarg :shape :reader shape)
   (%element-type :initarg :element-type :reader element-type)
   ;; The list of kernels that store into this buffer.
   (%inputs :initarg :inputs :accessor inputs :initform nil)
   ;; The list of kernels that load from this buffer.
   (%outputs :initarg :outputs :accessor outputs :initform nil)))

(defclass statement ()
  ((%operator :initarg :operator :reader operator)
   ;; The slots LOADS and STORES both contain a list, where each element is
   ;; either a symbol, or a cons cell whose car is a buffer and whose cdr
   ;; is a transformation.
   (%loads :initarg :loads :reader loads)
   (%stores :initarg :stores :reader stores)))

(defclass kernel ()
  ((%iteration-space :initarg :iteration-space :reader iteration-space)
   (%body :initarg :body :reader body)
   ;; The slots LOADS and STORES both contain a list, where each element is
   ;; a cons cell whose car is a buffer and whose cdr is a transformation.
   (%loads :initarg :loads :accessor loads)
   (%stores :initarg :stores :accessor stores)))

(defclass simple-kernel (kernel)
  ())

(defclass reduction-kernel (kernel)
  ((%operator :initarg :operator :reader operator)
   (%reduction-range :initarg :reduction-range :reader reduction-range)
   (%reduction-stores :initarg :reduction-stores :reader reduction-stores)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-buffer ((strided-array strided-array) (backend backend))
  (make-instance 'buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)))

(defmethod make-statement ((backend backend) &rest args)
  (apply #'make-instance 'statement args))

(defmethod make-simple-kernel ((backend backend) &rest args)
  (apply #'make-instance 'simple-kernel args))

(defmethod make-reduction-kernel ((backend backend) &rest args)
  (apply #'make-instance 'reduction-kernel args))

;;; We compute the loads and stores of a kernel during SHARED-INITIALIZE,
;;; such that one can use REINITIALIZE-INSTANCE to recompute them after the
;;; kernel body has changed.
(defmethod shared-initialize :after
    ((kernel kernel) slots &rest initargs)
  (declare (ignore slots initargs))
  (with-accessors ((loads loads) (stores stores)) kernel
    (multiple-value-setq (loads stores)
      (compute-kernel-loads-and-stores kernel))))

;;; In reduction kernels, not only the body can store values, but also the
;;; kernel itself.  Luckily, :after methods are run in least-specific-first
;;; order, so we can just append to the previously computed list.
(defmethod shared-initialize :after
    ((reduction-kernel reduction-kernel) slots &rest initargs)
  (declare (ignore slots initargs))
  (appendf (stores reduction-kernel)
           (remove-if #'null (reduction-stores reduction-kernel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pretty Printing

(defmethod print-object ((statement statement) stream)
  (format stream "{~{~S~^ ~} <- ~S ~{~S~^ ~}}"
          (stores statement)
          (operator statement)
          (loads statement)))

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
          (loop for (buffer . nil) in (loads kernel) do
            (pushnew kernel (outputs buffer)))
          (loop for (buffer . nil) in (stores kernel) do
            (pushnew kernel (inputs buffer))))))
    ;; Finally, return the buffers corresponding to the root nodes.
    (loop for strided-array in strided-arrays
          collect (gethash strided-array *buffer-table*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous Utilities

(defun reduction-value-symbol (n)
  (petalisp-memoization:with-vector-memoization (n)
    (intern
     (format nil "REDUCTION-VALUE-~D" n)
     :petalisp-ir)))

(defun ref= (a b)
  (and (eq (car a) (car b))
       (transformation-equal (cdr a) (cdr b))))

(defun make-padded-list (length &key padding tail)
  (nreverse
   (cons tail (make-list length :initial-element padding))))

(defun compute-kernel-loads-and-stores (kernel)
  (let ((all-loads '())
        (all-stores '()))
    (labels ((scan-statement (statement)
               (loop for load in (loads statement)
                     when (consp load)
                       do (pushnew load all-loads :test #'ref=))
               (loop for store in (stores statement)
                     when (consp store)
                       do (pushnew store all-stores :test #'ref=))))
      (mapc #'scan-statement (body kernel)))
    (values all-loads all-stores)))
