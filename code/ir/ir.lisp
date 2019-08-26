;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; A buffer represents a set of memory locations big enough to hold one
;;; element of type ELEMENT-TYPE for each index of the buffer's shape.
;;; Each buffer is written to by zero or more kernels and read from zero or
;;; more kernels.
(defstruct (buffer
            (:predicate bufferp)
            (:constructor %make-buffer))
  ;; The shape of the buffer.
  (shape nil :type shape)
  ;; The type code of all elements stored in the buffer.
  (ntype nil)
  ;; The list of kernels that store into this buffer.
  (inputs '() :type list)
  ;; The list of kernels that load from this buffer.
  (outputs '() :type list)
  (executedp nil :type boolean)
  (reusablep nil :type boolean)
  (storage nil))

(defun make-buffer (array)
  (etypecase array
    (array-immediate
     (%make-buffer
      :shape (shape array)
      :ntype (ntype array)
      :storage (storage array)
      :reusablep (reusablep array)
      :executedp t))
    (lazy-array
     (%make-buffer
      :shape (shape array)
      :reusablep t
      :ntype (ntype array)))))

;;; A kernel represents a computation that, for each element in its
;;; iteration space, reads from some buffers and writes to some buffers.
;;; By convention, the first range of the iteration space is always the
;;; reduction range.
(defstruct (kernel
            (:predicate kernelp))
  (iteration-space nil :type shape)
  (load-instructions nil :type list)
  (store-instructions nil :type list)
  (executedp nil :type boolean))

(declaim (inline kernel-reduction-range))
(defun kernel-reduction-range (kernel)
  (declare (kernel kernel))
  (first (shape-ranges (kernel-iteration-space kernel))))

;;; The behavior of a kernel is described by its iteration space and its
;;; instructions.  The instructions form a DAG, whose leaves are load
;;; instructions or references to iteration variables, and whose roots are
;;; store instructions.
;;;
;;; The instruction number of an instruction is an integer that is unique
;;; among all instructions of the current kernel.  Instruction numbers are
;;; handed out in depth first order of instruction dependencies, such that
;;; the roots (store instructions) have the highest numbers and that the
;;; leaf nodes (load and iref instructions) have the lowest numbers.  After
;;; modifications to the instruction graph, the numbers have to be
;;; recomputed.
;;;
;;; Each instruction input is a cons cell, whose cdr is another
;;; instruction, and whose car is an integer describing which of the
;;; multiple values of the cdr is referenced.
(defstruct (instruction
            (:predicate instructionp)
            (:constructor nil))
  (number 0 :type fixnum)
  (inputs '() :type list))

;;; A call instruction represents the application of a function to a set of
;;; values that are the result of other instructions.
(defstruct (call-instruction
            (:include instruction)
            (:predicate call-instruction-p)
            (:constructor make-call-instruction (operator inputs)))
  (operator nil :type (or function symbol)))

;;; We call an instruction an iterating instruction, if its behavior
;;; directly depends on the current element of the iteration space.
(defstruct (iterating-instruction
            (:include instruction)
            (:predicate iterating-instruction-p)
            (:constructor nil)
            (:conc-name instruction-))
  (transformation nil :type transformation))

;;; An iref instruction represents an access to elements of the iteration
;;; space itself.  Its transformation is a mapping from the iteration space
;;; to a rank one space.  Its value is the single integer that is the
;;; result of applying the transformation to the current iteration space.
(defstruct (iref-instruction
            (:include iterating-instruction)
            (:predicate iref-instruction-p)
            (:constructor make-iref-instruction (transformation))))

;;; A load instruction represents a read from main memory.  It returns a
;;; single value --- the entry of the buffer storage at the location
;;; specified by the current element of the iteration space and the load's
;;; transformation.
(defstruct (load-instruction
            (:include iterating-instruction)
            (:predicate load-instruction-p)
            (:constructor make-load-instruction (buffer transformation)))
  (buffer nil :type buffer))

;;; A store instruction represents a write to main memory.  It stores its
;;; one and only input at the entry of the buffer storage specified by the
;;; current element of the iteration space and the store instruction's
;;; transformation.  A store instruction returns zero values.
(defstruct (store-instruction
            (:include iterating-instruction)
            (:predicate store-instruction-p)
            (:constructor make-store-instruction (value buffer transformation
                                                  &aux (inputs (list value)))))
  (buffer nil :type buffer))

;;; A reduce instruction represents a binary tree reduction along the axis
;;; zero of the iteration space.  The operator of the reduce instruction is
;;; a function that takes twice as many arguments as the instruction has
;;; inputs, and returns as many values as the instruction has inputs.
(defstruct (reduce-instruction
            (:include instruction)
            (:predicate reduce-instruction-p)
            (:constructor make-reduce-instruction (operator inputs)))
  (operator nil :type (or function symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~S ~S"
            (petalisp.type-inference:type-specifier
             (buffer-ntype buffer))
            (buffer-shape buffer))))

(defmethod print-object ((kernel kernel) stream)
  (print-unreadable-object (kernel stream :type t :identity t)
    (format stream "~S"
            (kernel-iteration-space kernel))))

;;; This function is used during printing, to avoid excessive circularity.
(defun simplify-input (input)
  (destructuring-bind (value-n . instruction) input
    (cons value-n (instruction-number instruction))))

(defmethod print-object ((call-instruction call-instruction) stream)
  (print-unreadable-object (call-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number call-instruction)
            (call-instruction-operator call-instruction)
            (mapcar #'simplify-input (instruction-inputs call-instruction)))))

(defmethod print-object ((load-instruction load-instruction) stream)
  (print-unreadable-object (load-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number load-instruction)
            :buffer ;(load-instruction-buffer load-instruction)
            (instruction-transformation load-instruction))))

(defmethod print-object ((store-instruction store-instruction) stream)
  (print-unreadable-object (store-instruction stream :type t)
    (format stream "~S ~S ~S ~S"
            (instruction-number store-instruction)
            (simplify-input (first (instruction-inputs store-instruction)))
            :buffer ;(store-instruction-buffer store-instruction)
            (instruction-transformation store-instruction))))

(defmethod print-object ((iref-instruction iref-instruction) stream)
  (print-unreadable-object (iref-instruction stream :type t)
    (format stream "~S ~S"
            (instruction-number iref-instruction)
            (instruction-transformation iref-instruction))))

(defmethod print-object ((reduce-instruction reduce-instruction) stream)
  (print-unreadable-object (reduce-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number reduce-instruction)
            (reduce-instruction-operator reduce-instruction)
            (mapcar #'simplify-input
                    (instruction-inputs reduce-instruction)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mapping Functions

(declaim (inline map-buffer-inputs))
(defun map-buffer-inputs (function buffer)
  (declare (function function)
           (buffer buffer))
  (loop for kernel of-type kernel in (buffer-inputs buffer) do
    (funcall function kernel)))

(declaim (inline map-buffer-outputs))
(defun map-buffer-outputs (function buffer)
  (declare (function function)
           (buffer buffer))
  (loop for kernel of-type kernel in (buffer-outputs buffer) do
    (funcall function kernel)))

(declaim (inline map-kernel-store-instructions))
(defun map-kernel-store-instructions (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for store-instruction of-type store-instruction
          in (kernel-store-instructions kernel) do
    (funcall function store-instruction)))

(declaim (inline map-kernel-load-instructions))
(defun map-kernel-load-instructions (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for load-instruction of-type load-instruction
          in (kernel-load-instructions kernel) do
    (funcall function load-instruction)))

(declaim (inline map-kernel-inputs))
(defun map-kernel-inputs (function kernel)
  (declare (function function)
           (kernel kernel))
  (map-kernel-load-instructions
   (lambda (load-instruction)
     (funcall function (load-instruction-buffer load-instruction)))
   kernel))

(declaim (inline map-kernel-outputs))
(defun map-kernel-outputs (function kernel)
  (declare (function function)
           (kernel kernel))
  (map-kernel-store-instructions
   (lambda (store-instruction)
     (funcall function (store-instruction-buffer store-instruction)))
   kernel))

(declaim (inline map-instruction-inputs))
(defun map-instruction-inputs (function instruction)
  (declare (function function)
           (instruction instruction))
  (loop for (nil . input) in (instruction-inputs instruction) do
    (funcall function input)))

(defun map-buffers-and-kernels (buffer-fn kernel-fn root-buffers)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((process-buffer (buffer)
               (unless (gethash buffer table)
                 (setf (gethash buffer table) t)
                 (funcall buffer-fn buffer)
                 (map-buffer-inputs #'process-kernel buffer)))
             (process-kernel (kernel)
               (funcall kernel-fn kernel)
               (map-kernel-inputs #'process-buffer kernel)))
      (mapc #'process-buffer root-buffers))))

(defun map-buffers (function root-buffers)
  (map-buffers-and-kernels function #'identity root-buffers))

(defun map-kernels (function root-buffers)
  (map-buffers-and-kernels #'identity function root-buffers))

(defun map-instructions (function kernel)
  (map-kernel-store-instructions
   (lambda (store-instruction)
     (map-instruction-tree function store-instruction))
   kernel))

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
;;; Rotating

(declaim (inline transform-instruction-input))
(defun transform-instruction-input (instruction transformation)
  (declare (instruction instruction)
           (transformation transformation))
  (when (iterating-instruction-p instruction)
    (setf (instruction-transformation instruction)
          (compose-transformations
           (instruction-transformation instruction)
           transformation)))
  instruction)

(declaim (inline transform-instruction-output))
(defun transform-instruction-output (instruction transformation)
  (declare (instruction instruction)
           (transformation transformation))
  (when (iterating-instruction-p instruction)
    (setf (instruction-transformation instruction)
          (compose-transformations
           transformation
           (instruction-transformation instruction))))
  instruction)


(defun transform-buffer (buffer transformation)
  (declare (buffer buffer)
           (transformation transformation))
  (setf (buffer-shape buffer)
        (transform (buffer-shape buffer) transformation))
  ;; After rotating a buffer, rotate all loads and stores referencing the
  ;; buffer to preserve the semantics of the IR.
  (map-buffer-inputs
   (lambda (kernel)
     (map-kernel-store-instructions
      (lambda (store-instruction)
        (when (eq (store-instruction-buffer store-instruction) buffer)
          (transform-instruction-output store-instruction transformation)))
      kernel))
   buffer)
  (map-buffer-outputs
   (lambda (kernel)
     (map-kernel-load-instructions
      (lambda (load-instruction)
        (when (eq (load-instruction-buffer load-instruction) buffer)
          (transform-instruction-output load-instruction transformation)))
      kernel))
   buffer)
  buffer)

(defun transform-kernel (kernel transformation)
  (declare (kernel kernel)
           (transformation transformation))
  (unless (identity-transformation-p transformation)
    (setf (kernel-iteration-space kernel)
          (transform (kernel-iteration-space kernel) transformation))
    (let ((inverse (invert-transformation transformation)))
      (map-instructions
       (lambda (instruction)
         (transform-instruction-input instruction inverse))
       kernel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

(defun kernel-number-of-loads (kernel)
  (declare (kernel kernel))
  (let ((counter 0))
    (declare (fixnum counter))
    (map-kernel-load-instructions
     (lambda (_) (declare (ignore _))
       (incf counter))
     kernel)
    counter))

(defun kernel-number-of-stores (kernel)
  (declare (kernel kernel))
  (let ((counter 0))
    (declare (fixnum counter))
    (map-kernel-store-instructions
     (lambda (_) (declare (ignore _))
       (incf counter))
     kernel)
    counter))

(defun kernel-highest-instruction-number (kernel)
  (declare (kernel kernel))
  (let ((max 0))
    ;; This function exploits that the numbers are handed out in
    ;; depth-first order, starting from the leaf instructions.  So we know
    ;; that the highest instruction number must be somewhere among the
    ;; store instructions.
    (map-kernel-store-instructions
     (lambda (store-instruction)
       (maxf max (instruction-number store-instruction)))
     kernel)
    max))

;; TODO: This function is only used to allocate a vector of buffers during
;; scheduling, so there is some opportunity to avoid consing.
(defun kernel-buffers (kernel)
  (let ((buffers '()))
    (map-kernel-load-instructions
     (lambda (load-instruction)
       (pushnew (load-instruction-buffer load-instruction) buffers))
     kernel)
    (map-kernel-store-instructions
     (lambda (store-instruction)
       (pushnew (store-instruction-buffer store-instruction) buffers))
     kernel)
    buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assigning Instruction Numbers

(defun assign-instruction-numbers (kernel)
  ;; Step 1 - set all instruction numbers to -1.
  (labels ((clear-instruction-numbers (instruction)
             (unless (= -1 (instruction-number instruction))
               (map-instruction-inputs #'clear-instruction-numbers instruction)
               (setf (instruction-number instruction) -1))))
    (map-kernel-store-instructions #'clear-instruction-numbers kernel))
  ;; Step 2 - assign new instruction numbers.
  (let ((n -1))
    (labels ((assign-instruction-numbers (instruction)
               (when (= -1 (instruction-number instruction))
                 (setf (instruction-number instruction) -2)
                 (map-instruction-inputs #'assign-instruction-numbers instruction)
                 (setf (instruction-number instruction) (incf n)))))
      (map-kernel-store-instructions #'assign-instruction-numbers kernel))))

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
  (map-buffers #'normalize-buffer root-buffers)
  (map-kernels #'normalize-kernel root-buffers))

(defun normalize-buffer (buffer)
  (transform-buffer buffer (collapsing-transformation (buffer-shape buffer))))

(defun normalize-kernel (kernel)
  (transform-kernel kernel (collapsing-transformation (kernel-iteration-space kernel))))
