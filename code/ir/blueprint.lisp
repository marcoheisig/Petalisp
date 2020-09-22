;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; A blueprint is an s-expression made of ucons cells.  It contains all
;;; the information necessary to compute an efficient evaluation function
;;; for this kernel.  The idea is that blueprints can be used to cache
;;; compiled evaluation functions.

(declaim (type list *buffers*))
(defvar *buffers*)

(defvar *function-counter*)

(defun kernel-blueprint (kernel)
  (let ((*buffers* (kernel-buffers kernel))
        (*function-counter* -1))
    (ucons:ulist
     (ucons:umapcar #'range-blueprint (shape-ranges (kernel-iteration-space kernel)))
     (ucons:umapcar #'buffer-blueprint *buffers*)
     ;; Now generate the blueprints for all instructions in the kernel
     (let ((vector (kernel-instruction-vector kernel))
           (result '()))
       (loop for index from (1- (length vector)) downto 0 do
         (let ((instruction (svref vector index)))
           (setf result (ucons:ucons (instruction-blueprint instruction) result))))
       result))))

(defun range-blueprint (range)
  (declare (range range))
  (cond ((size-one-range-p range)
         :single)
        ((= 1 (range-step range))
         :contiguous)
        (t
         :strided)))

(defun buffer-blueprint (buffer)
  (ucons:ulist
   (buffer-ntype buffer)
   (shape-rank (buffer-shape buffer))))

(defun transformation-blueprint (transformation)
  (let ((result '()))
    (map-transformation-outputs
     (lambda (output-index input-index scaling offset)
       (declare (ignore output-index))
       (setf result (ucons:ucons
                     (ucons:ulist input-index scaling offset) result)))
     transformation
     :from-end t)
    result))

(defun operator-blueprint (operator)
  (etypecase operator
    (function (incf *function-counter*))
    (symbol operator)))

(defun value-blueprint (value)
  (destructuring-bind (value-n . instruction) value
    (ucons:ulist value-n (instruction-number instruction))))

(defun buffer-number (buffer)
  (or (position buffer *buffers*)
      (error "No such buffer ~S." buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Blueprints

(defgeneric instruction-blueprint (instruction))

(defmethod instruction-blueprint ((call-instruction call-instruction))
  (ucons:ulist*
   :call
   (operator-blueprint (call-instruction-operator call-instruction))
   (ucons:umapcar #'value-blueprint (instruction-inputs call-instruction))))

(defmethod instruction-blueprint ((load-instruction load-instruction))
  (ucons:ulist*
   :load
   (buffer-number (load-instruction-buffer load-instruction))
   (transformation-blueprint (instruction-transformation load-instruction))))

(defmethod instruction-blueprint ((store-instruction store-instruction))
  (ucons:ulist*
   :store
   (value-blueprint (first (instruction-inputs store-instruction)))
   (buffer-number (store-instruction-buffer store-instruction))
   (transformation-blueprint (instruction-transformation store-instruction))))

(defmethod instruction-blueprint ((iref-instruction iref-instruction))
  (block nil
    (map-transformation-outputs
     (lambda (output-index input-index scaling offset)
       (declare (ignore output-index))
       (return (ucons:ulist :iref input-index scaling offset)))
     (instruction-transformation iref-instruction))))

;;; Return as multiple values
;;;
;;; 1. A list of range descriptions.
;;;
;;; 2. A list of array ntypes.
;;;
;;; 3. A list of instructions.

(defun parse-kernel-blueprint (blueprint)
  (destructuring-bind (range-info array-info instructions)
      (ucons:tree-from-utree blueprint)
    (values
     range-info
     (loop for (element-ntype rank) in array-info
           collect
           `(simple-array
             ,(petalisp.type-inference:type-specifier element-ntype)
             ,(loop repeat rank collect '*)))
     instructions)))
