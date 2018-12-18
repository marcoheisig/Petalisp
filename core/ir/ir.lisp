;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-buffer (strided-array backend))

(defgeneric make-kernel (backend &key iteration-space loads stores reduction-range))

(defgeneric map-instruction-inputs (function instruction))

(defgeneric rotate-buffer (buffer transformation)
  (:argument-precedence-order transformation buffer))

(defgeneric rotate-kernel (kernel transformation)
  (:argument-precedence-order transformation kernel))

(defgeneric rotate-instruction-input (instruction transformation)
  (:argument-precedence-order transformation instruction))

(defgeneric rotate-instruction-output (instruction transformation)
  (:argument-precedence-order transformation instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

;;; A buffer represents a set of memory locations big enough to hold one
;;; element of type ELEMENT-TYPE for each index of the buffer's shape.
;;; Each buffer is written to by zero or more kernels and read from zero or
;;; more kernels.
(defclass buffer ()
  ((%shape :initarg :shape :accessor buffer-shape)
   (%element-type :initarg :element-type :reader element-type)
   ;; The list of kernels that store into this buffer.
   (%inputs :initarg :inputs :accessor inputs :initform nil)
   ;; The list of kernels that load from this buffer.
   (%outputs :initarg :outputs :accessor outputs :initform nil)))

;;; A kernel represents a computation that, for each element in its
;;; iteration space, reads from some buffers and writes to some buffers.
;;; Its exact behavior is determined by its instructions, which are
;;; accessible via the slots LOADS and STORES.
(defclass kernel ()
  ((%iteration-space :initarg :iteration-space :accessor iteration-space)
   (%reduction-range :initarg :reduction-range :reader reduction-range)
   (%loads :initarg :loads :accessor loads)
   (%stores :initarg :stores :accessor stores)))

;;; The behavior of a kernel is described by its iteration space and its
;;; instructions.  The instructions form a DAG, whose leaves are loads or
;;; references to iteration variables, and whose roots are store
;;; instructions.
;;;
;;; The instruction number of an instruction is an integer that is unique
;;; among all instructions of the current kernel.  Instruction numbers are
;;; handed out in depth first order of instruction dependencies, such that
;;; the roots (store instructions) have the highest numbers and that the
;;; leaf nodes (load and iref instructions) have the lowest numbers.
(defclass instruction ()
  ((%number :initform (next-instruction-number) :accessor instruction-number)))

;;; We call an instruction an iterating instruction, if its behavior
;;; directly depends on the current element of the iteration space.
(defclass iterating-instruction (instruction)
  ((%transformation :initarg :transformation :accessor transformation)))

;;; A call instruction represents the application of a function to a set of
;;; values that are the result of other instructions.  Each argument is
;;; represented as a cons cell, whose cdr is another instruction, and whose
;;; car is an integer describing which of the multiple values of the cdr is
;;; to be used.
(defclass call-instruction (instruction)
  ((%operator :initarg :operator :reader operator)
   (%arguments :initarg :arguments :reader arguments)))

;;; A load instruction represents a read from main memory.  It returns a
;;; single value --- the entry of the buffer storage at the location
;;; specified by the current element of the iteration space and the load's
;;; transformation.
(defclass load-instruction (iterating-instruction)
  ((%buffer :initarg :buffer :reader buffer)))

;;; A store instruction represents a write to main memory.  It stores the
;;; given value (represented as a cons cell, just like the arguments of a
;;; call instruction) at the entry of the buffer storage specified by the
;;; current element of the iteration space and the store instruction's
;;; transformation.  A store instruction returns zero values.
(defclass store-instruction (iterating-instruction)
  ((%value :initarg :value :reader value)
   (%buffer :initarg :buffer :reader buffer)))

;;; An iref instruction represents an access to elements of the iteration
;;; space itself.  Its transformation is a mapping from the iteration space
;;; to a rank one space.  Its value is the single integer that is the
;;; result of applying the transformation to the current iteration space.
(defclass iref-instruction (iterating-instruction)
  ())

;;; A reduce instruction represents a binary tree reduction along the axis
;;; zero of the iteration space.  Each argument is represented as a cons
;;; cell, whose cdr is another instruction, and whose car is an integer
;;; describing which of the multiple values of the cdr is to be used.  The
;;; operator of the reduce instruction is a function that takes twice as
;;; many arguments as the instruction itself, and returns as many values as
;;; the instruction has arguments.
(defclass reduce-instruction (iterating-instruction)
  ((%operator :initarg :operator :reader operator)
   (%arguments :initarg :arguments :reader arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~S ~S"
            (element-type buffer)
            (buffer-shape buffer))))

(defmethod print-object ((kernel kernel) stream)
  (print-unreadable-object (kernel stream :type t :identity t)
    (format stream "~S ~S"
            (iteration-space kernel)
            (reduction-range kernel))))

(defun simplify-argument (argument)
  (destructuring-bind (value-n . instruction) argument
    (cons value-n (instruction-number instruction))))

(defmethod print-object ((call-instruction call-instruction) stream)
  (print-unreadable-object (call-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number call-instruction)
            (operator call-instruction)
            (mapcar #'simplify-argument (arguments call-instruction)))))

(defmethod print-object ((load-instruction load-instruction) stream)
  (print-unreadable-object (load-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number load-instruction)
            :buffer ;(buffer load-instruction)
            (transformation load-instruction))))

(defmethod print-object ((store-instruction store-instruction) stream)
  (print-unreadable-object (store-instruction stream :type t)
    (format stream "~S ~S ~S ~S"
            (instruction-number store-instruction)
            (simplify-argument (value store-instruction))
            :buffer ;(buffer store-instruction)
            (transformation store-instruction))))

(defmethod print-object ((iref-instruction iref-instruction) stream)
  (print-unreadable-object (iref-instruction stream :type t)
    (format stream "~S ~S"
            (instruction-number iref-instruction)
            (transformation iref-instruction))))

(defmethod print-object ((reduce-instruction reduce-instruction) stream)
  (print-unreadable-object (reduce-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number reduce-instruction)
            (operator reduce-instruction)
            (mapcar #'simplify-argument (arguments reduce-instruction)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-buffer ((strided-array strided-array) (backend backend))
  (make-instance 'buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)))

(defmethod make-kernel ((backend backend) &rest args)
  (apply #'make-instance 'kernel args))

;;; MAP-INSTRUCTION-INPUTS

(defmethod map-instruction-inputs ((function function) (call-instruction call-instruction))
  (loop for (nil . input) in (arguments call-instruction) do
    (funcall function input)))

(defmethod map-instruction-inputs ((function function) (reduce-instruction reduce-instruction))
  (loop for (nil . input) in (arguments reduce-instruction) do
    (funcall function input)))

(defmethod map-instruction-inputs ((function function) (store-instruction store-instruction))
  (funcall function (cdr (value store-instruction))))

(defmethod map-instruction-inputs ((function function) (load-instruction load-instruction))
  (values))

(defmethod map-instruction-inputs ((function function) (iref-instruction iref-instruction))
  (values))

;;; ROTATE-BUFFER

(defmethod rotate-buffer ((buffer buffer) (transformation transformation))
  (setf (buffer-shape buffer)
        (transform (buffer-shape buffer) transformation)))

;;; After rotating a buffer, rotate all loads and stores referencing the
;;; buffer to preserve the semantics of the IR.
(defmethod rotate-buffer :after ((buffer buffer) (transformation transformation))
  (loop for kernel in (inputs buffer) do
    (loop for store in (stores kernel) do
      (when (eq (buffer store) buffer)
        (rotate-instruction-output store transformation))))
  (loop for kernel in (outputs buffer) do
    (loop for load in (loads kernel) do
      (when (eq (buffer load) buffer)
        (rotate-instruction-output load transformation)))))

;;; ROTATE-KERNEL

(defmethod rotate-kernel ((kernel kernel) (transformation identity-transformation))
  (declare (ignore kernel transformation)))

(defmethod rotate-kernel ((kernel kernel) (transformation transformation))
  (setf (iteration-space kernel)
        (transform (iteration-space kernel) transformation))
  (let ((inverse (invert-transformation transformation)))
    (map-instructions
     (lambda (instruction)
       (rotate-instruction-input instruction inverse))
     kernel)))

;;; ROTATE-INSTRUCTION-*

(defmethod rotate-instruction-input ((instruction instruction) (transformation transformation))
  (values))

(defmethod rotate-instruction-input ((iterating-instruction iterating-instruction)
                                     (transformation transformation))
  (setf (transformation iterating-instruction)
        (compose-transformations
         (transformation iterating-instruction)
         transformation)))

(defmethod rotate-instruction-output ((instruction instruction) (transformation transformation))
  (values))


(defmethod rotate-instruction-output ((iterating-instruction iterating-instruction)
                                      (transformation transformation))
  (setf (transformation iterating-instruction)
        (compose-transformations
         transformation
         (transformation iterating-instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(defgeneric reduction-kernel-p (object)
  (:method ((object t)) nil)
  (:method ((kernel kernel))
    (not (null (reduction-range kernel)))))

(defun kernel-buffers (kernel)
  (let ((buffers '()))
    (loop for load in (loads kernel) do
      (pushnew (buffer load) buffers))
    (loop for store in (stores kernel) do
      (pushnew (buffer store) buffers))
    buffers))

(defun map-buffers-and-kernels (buffer-fn kernel-fn root-buffers )
  (let ((table (make-hash-table :test #'eq)))
    (labels ((process-buffer (buffer)
               (unless (gethash buffer table)
                 (setf (gethash buffer table) t)
                 (funcall buffer-fn buffer)
                 (mapc #'process-kernel (inputs buffer))))
             (process-kernel (kernel)
               (unless (gethash kernel table)
                 (setf (gethash kernel table) t)
                 (funcall kernel-fn kernel)
                 (loop for load in (loads kernel) do
                   (process-buffer (buffer load))))))
      (mapc #'process-buffer root-buffers))))

(defun map-buffers (function root-buffers)
  (map-buffers-and-kernels function #'identity root-buffers))

(defun map-kernels (function root-buffers)
  (map-buffers-and-kernels #'identity function root-buffers))

(defun map-instructions (function kernel)
  (loop for store in (petalisp-ir:stores kernel) do
    (map-instruction-tree function store)))

(defun map-instruction-tree (function root-instruction)
  (labels ((process-node (instruction n)
             (let ((new-n (instruction-number instruction)))
               (when (< new-n n)
                 (funcall function instruction)
                 (map-instruction-inputs
                  (lambda (next) (process-node next new-n))
                  instruction)))))
    (process-node root-instruction most-positive-fixnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assigning Instruction Numbers

(defvar *instruction-counter*)

(defun next-instruction-number ()
  (incf *instruction-counter*))

(defmacro with-instruction-numbering (&body body)
  `(let ((*instruction-counter* -1))
     ,@body))

;;; This function exploits that the numbers are handed out starting from
;;; the leaf instructions.  So we know that the highest instruction number
;;; must be somewhere at the root instructions.
(defun highest-instruction-number (kernel)
  (max (loop for store in (petalisp-ir:stores kernel)
             maximize (petalisp-ir:instruction-number store))))

(defun update-instruction-numbers (kernel)
  ;; Step 1 - set all instruction numbers to NIL
  (labels ((clear-instruction-numbers (instruction)
             (unless (null (instruction-number instruction))
               (map-instruction-inputs #'clear-instruction-numbers instruction)
               (setf (instruction-number instruction) nil))))
    (mapc #'clear-instruction-numbers (stores kernel)))
  ;; Step 2 - assign new instruction numbers
  (let ((n -1))
    (labels ((assign-instruction-numbers (instruction)
               (when (null (instruction-number instruction))
                 (map-instruction-inputs #'assign-instruction-numbers instruction)
                 (setf (instruction-number instruction) (incf n)))))
      (mapc #'assign-instruction-numbers (stores kernel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Normalization
;;;
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

(defun normalize-ir (root-buffers)
  (map-buffers #'normalize-buffer root-buffers))

(defun normalize-buffer (buffer)
  (rotate-buffer buffer (collapsing-transformation (buffer-shape buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Locality Optimization
;;;
;;; Even after IR normalization, there remains another degree of freedom -
;;; the order in which the iteration space of each kernel is traversed.  By
;;; construction, kernels have no data dependencies, so the order does not
;;; matter.
;;;
;;; The purpose of this IR transformation is to improve the data locality
;;; by interchanging the ranges of the iteration space.  By convention,
;;; backends treat the first range as the innermost loop and the last range
;;; as the outermost loop.  So the goal is to move the loops with the
;;; highest locality to the front.
;;;
;;; One invariant that is not touched by this transformation is that if the
;;; kernel is a reduction kernel, then the first range of the iteration
;;; space is always the range of the reduction.  Otherwise, we would need
;;; to allocate an awful lot of temporaries for the reduction.

(defun optimize-locality (root-buffers)
  (map-kernels #'optimize-kernel-locality root-buffers))

(defun optimize-kernel-locality (kernel)
  (rotate-kernel kernel (identity-transformation (rank (iteration-space kernel)))))
