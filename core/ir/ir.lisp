;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

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
   (%loads :initarg :loads :accessor loads)
   (%stores :initarg :stores :accessor stores)
   (%reduction-stores :initarg :reduction-stores :accessor reduction-stores)))

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

;;; A reduction store instruction behaves just like a store instructions,
;;; but is expected to be run outside of the innermost loop.  Its value
;;; must be a reference to a reduce instruction.
(defclass reduction-store-instruction (store-instruction)
  ())

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
;;; Constructors

(defgeneric make-buffer (strided-array backend))

(defgeneric make-kernel (backend &key iteration-space loads stores reduction-stores))

(defmethod make-buffer ((strided-array strided-array) (backend backend))
  (make-instance 'buffer
    :shape (shape strided-array)
    :element-type (element-type strided-array)))

(defmethod make-kernel ((backend backend) &rest args)
  (apply #'make-instance 'kernel args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(defgeneric reduction-kernel-p (object)
  (:method ((object t)) nil)
  (:method ((kernel kernel))
    (not (null (reduction-stores kernel)))))

(defun kernel-reduce-instructions (kernel)
  (let ((result '()))
    (loop for reduction-store in (petalisp-ir:reduction-stores kernel) do
      (pushnew (cdr (petalisp-ir:value reduction-store)) result))
    result))

(defun kernel-buffers (kernel)
  (let ((buffers '()))
    (loop for load in (loads kernel) do
      (pushnew (buffer load) buffers))
    (loop for store in (stores kernel) do
      (pushnew (buffer store) buffers))
    (loop for reduction-store in (reduction-stores kernel) do
      (pushnew (buffer reduction-store) buffers))
    buffers))

(defun map-buffers (function root-buffers)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((process-buffer (buffer)
               (unless (gethash buffer table)
                 (setf (gethash buffer table) t)
                 (funcall function buffer)
                 (loop for kernel in (inputs buffer) do
                   (loop for load in (loads kernel) do
                     (process-buffer (buffer load)))))))
      (mapc #'process-buffer root-buffers))))

(defgeneric map-instruction-inputs (function instruction)
  (:method ((function function) (call-instruction call-instruction))
    (loop for (nil . input) in (arguments call-instruction) do
      (funcall function input)))
  (:method ((function function) (reduce-instruction reduce-instruction))
    (loop for (nil . input) in (arguments reduce-instruction) do
      (funcall function input)))
  (:method ((function function) (store-instruction store-instruction))
    (funcall function (cdr (value store-instruction))))
  (:method ((function function) (load-instruction load-instruction))
    (values))
  (:method ((function function) (iref-instruction iref-instruction))
    (values)))

(defun map-instructions (function kernel)
  (labels ((process-instruction (instruction n)
             (let ((new-n (instruction-number instruction)))
               (when (< new-n n)
                 (funcall function instruction)
                 (map-instruction-inputs
                  (lambda (next) (process-instruction next new-n))
                  instruction)))))
    (loop for store in (petalisp-ir:stores kernel) do
      (process-instruction store most-positive-fixnum))
    (loop for reduction-store in (petalisp-ir:reduction-stores kernel) do
      (process-instruction reduction-store most-positive-fixnum))))

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
             maximize (petalisp-ir:instruction-number store))
       (loop for store in (petalisp-ir:reduction-stores kernel)
             maximize (petalisp-ir:instruction-number store))))

(defun update-instruction-numbers (kernel)
  ;; Step 1 - set all instruction numbers to NIL
  (labels ((clear-instruction-numbers (instruction)
             (unless (null (instruction-number instruction))
               (map-instruction-inputs #'clear-instruction-numbers instruction)
               (setf (instruction-number instruction) nil))))
    (mapc #'clear-instruction-numbers (stores kernel))
    (mapc #'clear-instruction-numbers (reduction-stores kernel)))
  ;; Step 2 - assign new instruction numbers
  (let ((n -1))
    (labels ((assign-instruction-numbers (instruction)
               (when (null (instruction-number instruction))
                 (map-instruction-inputs #'assign-instruction-numbers instruction)
                 (setf (instruction-number instruction) (incf n)))))
      (mapc #'assign-instruction-numbers (stores kernel))
      (mapc #'assign-instruction-numbers (reduction-stores kernel)))))

