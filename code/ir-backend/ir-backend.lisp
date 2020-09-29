;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir-backend)

;;; The purpose of the IR backend is to check that the IR conversion
;;; preserves semantics.  It is similar to the reference backend, but
;;; evaluates kernels instead of individual strided arrays.

(defclass ir-backend (backend)
  ())

(defun make-ir-backend ()
  (make-instance 'ir-backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Flow Graph Evaluation

(defvar *nodes*)

(defvar *worklist*)

(defstruct node
  (kernel nil :type kernel)
  (dependencies '() :type list)
  (users '() :type list))

(defmethod compute-immediates ((lazy-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (ir-from-lazy-arrays lazy-arrays))
        (*nodes* (make-hash-table :test #'eq))
        (*worklist* '()))
    (map-buffers-and-kernels
     ;; Ensure that each buffer has an attached storage array.
     (lambda (buffer)
       (unless (buffer-storage buffer)
         (setf (buffer-storage buffer)
               (make-array
                (shape-dimensions (buffer-shape buffer))
                :element-type (petalisp.type-inference:type-specifier
                               (buffer-ntype buffer))))))
     ;; Ensure that the dependencies of each kernel are set up properly.
     (lambda (kernel)
       (let ((node (ensure-node kernel)))
         (map-kernel-inputs
          (lambda (buffer)
            (map-buffer-inputs
             (lambda (other-kernel)
               (let ((other-node (ensure-node other-kernel)))
                 (pushnew other-node (node-dependencies node))
                 (pushnew node (node-users other-node))))
             buffer))
          kernel)
         (when (null (node-dependencies node))
           (push node *worklist*))))
     root-buffers)
    (loop until (null *worklist*) do
      (execute-node (pop *worklist*)))
    (mapcar #'immediate-from-buffer root-buffers)))

(defun ensure-node (kernel)
  (or (gethash kernel *nodes*)
      (setf (gethash kernel *nodes*)
            (make-node :kernel kernel))))

(defun execute-node (node)
  (execute-kernel (node-kernel node))
  (loop for other-node in (node-users node) do
    (with-accessors ((dependencies node-dependencies)) other-node
      (setf dependencies (remove node dependencies))
      (when (null dependencies)
        (push other-node *worklist*)))))

(defun immediate-from-buffer (buffer)
  (make-instance 'array-immediate
    :ntype (buffer-ntype buffer)
    :shape (buffer-shape buffer)
    :storage (buffer-storage buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Execution

;;; An array, mapping from instruction numbers to lists of instruction
;;; values.
(defvar *instruction-values-cache*)

;;; The current kernel.
(defvar *kernel*)

;;; The current loop index.
(defvar *index*)

(defun make-instruction-values-cache (kernel)
  (make-array (1+ (kernel-highest-instruction-number kernel))
              :initial-element 0))

(defun instruction-values-cache (instruction)
  (aref *instruction-values-cache* (instruction-number instruction)))

(defun (setf instruction-values-cache) (value instruction)
  (setf (aref *instruction-values-cache* (instruction-number instruction))
        value))

(defun clear-instruction-values-cache ()
  (fill *instruction-values-cache* 0))

(defmethod execute-kernel (kernel)
  (let* ((*kernel* kernel)
         (*instruction-values-cache* (make-instruction-values-cache kernel)))
    (map-shape
     (lambda (index)
       (let ((*index* index))
         ;; Clear the cached values of the previous iteration.
         (clear-instruction-values-cache)
         ;; Evaluate all instructions with side-effects (= store
         ;; instructions) and their dependencies.
         (map-kernel-store-instructions #'instruction-values kernel)))
     (kernel-iteration-space kernel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execution of Instructions

;;; The generic function INSTRUCTION-VALUES returns the values of the given
;;; instruction, as a list.  Some instructions, e.g., loads, are influenced
;;; by the special variable *INDEX*.
(defgeneric instruction-values (instruction))

;;; Instruction Value Caching
(defmethod instruction-values :around
    ((instruction instruction))
  (let ((cache (instruction-values-cache instruction)))
    (if (listp cache)
        cache
        (setf (instruction-values-cache instruction)
              (call-next-method)))))

(defmethod instruction-values ((call-instruction call-instruction))
  (multiple-value-list
   (apply (call-instruction-operator call-instruction)
          (loop for (value-n . instruction)
                  in (instruction-inputs call-instruction)
                collect (nth value-n (instruction-values instruction))))))

(defmethod instruction-values ((load-instruction load-instruction))
  (list
   (apply #'aref
          (buffer-storage (load-instruction-buffer load-instruction))
          (transform *index* (instruction-transformation load-instruction)))))

(defmethod instruction-values ((store-instruction store-instruction))
  (setf (apply
         #'aref
         (buffer-storage (store-instruction-buffer store-instruction))
         (transform *index* (instruction-transformation store-instruction)))
        (destructuring-bind ((value-n . instruction))
            (instruction-inputs store-instruction)
          (nth value-n (instruction-values instruction))))
  (list))

(defmethod instruction-values ((iref-instruction iref-instruction))
  (transform *index* (instruction-transformation iref-instruction)))
