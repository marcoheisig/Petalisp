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
  ((%shape :initarg :shape :accessor shape)
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
  ((%number :initarg :number :accessor instruction-number)))

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
;;; space itself.  It returns a single value --- the integer obtained by
;;; taking the element denoted by AXIS of the index that is the result of
;;; transforming the current element of the iteration space with the iref's
;;; transformation.
(defclass iref-instruction (iterating-instruction)
  ((%axis :initarg :axis :reader axis)))

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

(defun map-instructions (function kernel)
  (labels ((process-instruction (instruction n)
             (let ((n-new (instruction-number instruction)))
               (when (< n-new n)
                 (funcall function instruction)
                 (typecase instruction
                   (call-instruction
                    (loop for (nil . instruction) in (arguments instruction) do
                      (process-instruction instruction n-new)))
                   (store-instruction
                    (process-instruction (cdr (value instruction)) n-new)))))))
    (loop for store in (petalisp-ir:stores kernel) do
      (process-instruction store most-positive-fixnum))
    (loop for store in (petalisp-ir:reduction-stores kernel) do
      (process-instruction store most-negative-fixnum))))

(defun reduce-instructions (kernel)
  (let ((result '()))
    (loop for reduction-store in (petalisp-ir:reduction-stores kernel) do
      (pushnew (cdr (petalisp-ir:value reduction-store)) result))
    result))
