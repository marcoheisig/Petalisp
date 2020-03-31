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

(defmethod compute-immediates ((lazy-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (ir-from-lazy-arrays lazy-arrays)))
    (normalize-ir root-buffers)
    (mapc #'execute-buffer root-buffers)
    (mapcar #'immediate-from-buffer root-buffers)))

(defun immediate-from-buffer (buffer)
  (make-instance 'array-immediate
    :ntype (buffer-ntype buffer)
    :shape (buffer-shape buffer)
    :storage (buffer-storage buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer Execution

(defun execute-buffer (buffer)
  (unless (buffer-executedp buffer)
    (setf (buffer-executedp buffer) t)
    (setf (buffer-storage buffer)
          (make-array
           (mapcar #'range-size (shape-ranges (buffer-shape buffer)))
           :element-type (petalisp.type-inference:type-specifier
                          (buffer-ntype buffer))))
    (map-buffer-inputs #'execute-kernel buffer)))

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
  (unless (kernel-executedp kernel)
    (setf (kernel-executedp kernel) t)
    (map-kernel-inputs #'execute-buffer kernel)
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
       (shrink-shape
        (kernel-iteration-space kernel))))))

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
  (setf (apply #'aref
               (buffer-storage (store-instruction-buffer store-instruction))
               (transform (cons 0 *index*) (instruction-transformation store-instruction)))
        (destructuring-bind ((value-n . instruction))
            (instruction-inputs store-instruction)
          (nth value-n (instruction-values instruction))))
  (list))

(defmethod instruction-values ((reduce-instruction reduce-instruction))
  (let ((k (length (instruction-inputs reduce-instruction))))
    (labels ((divide-and-conquer (range)
               (if (size-one-range-p range)
                   (let ((*index* (cons (range-start range) *index*)))
                     ;; *index* is changed, so we also have to clear the cache.
                     (clear-instruction-values-cache)
                     (loop for (value-n . instruction)
                             in (instruction-inputs reduce-instruction)
                           collect (nth value-n (instruction-values instruction))))
                   (multiple-value-bind (left right)
                       (split-range range)
                     (subseq
                      (multiple-value-list
                       (multiple-value-call (reduce-instruction-operator reduce-instruction)
                         (values-list (divide-and-conquer left))
                         (values-list (divide-and-conquer right))))
                      0 k)))))
      (divide-and-conquer (kernel-reduction-range *kernel*)))))

(defmethod instruction-values ((iref-instruction iref-instruction))
  (transform *index* (instruction-transformation iref-instruction)))
