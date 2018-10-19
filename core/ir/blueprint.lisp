;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;; A blueprint is an s-expression made of ucons cells.  It contains all
;;; the information necessary to compute an efficient evaluation function
;;; for this kernel.  The idea is that blueprints can be used to cache
;;; compiled evaluation functions.

(defgeneric blueprint (kernel))

;;; A list of buffers referenced by the current kernel.
(defvar *buffers*)

(defun compute-kernel-buffers (kernel)
  (let ((buffers '()))
    (loop for load in (loads kernel) do
      (pushnew (buffer load) buffers))
    (loop for store in (stores kernel) do
      (pushnew (buffer store) buffers))
    (loop for store in (reduction-stores kernel) do
      (pushnew (buffer store) buffers))
    buffers))

(defmethod blueprint :around ((kernel kernel))
  (let ((*buffers* (compute-kernel-buffers kernel)))
    (call-next-method)))

(defmethod blueprint ((kernel kernel))
  (ucons:ulist
   (ucons:umapcar #'blueprint *buffers*)
   (ucons:umapcar #'blueprint (ranges (iteration-space kernel)))
   ;; Now generate the blueprints for all instructions in the kernel
   (let* ((size (1+ (highest-instruction-number kernel)))
          (instruction-blueprints (make-array size))
          (result '()))
     (map-instructions
      (lambda (instruction)
        (let ((index (instruction-number instruction)))
          (setf (aref instruction-blueprints index)
                (blueprint instruction))))
      kernel)
     (loop for index from (1- size) downto 0 do
       (let ((instruction (aref instruction-blueprints index)))
         (setf result (ucons:ucons instruction result))))
     result)))

(defmethod blueprint ((buffer buffer))
  (ucons:ulist 'simple-array (element-type buffer)))

;;; Return an ulist with the following elements:
;;;
;;; 1. The number of bits necessary to describe the range size.
;;;
;;; 2. The number of bits necessary to describe the range step.
;;;
;;; 3. The type of the iteration variable, either integer or fixnum.
(defmethod blueprint ((range range))
  (multiple-value-bind (start step end)
      (range-start-step-end range)
    (ucons:ulist
     (integer-length (size range))
     (integer-length step)
     (if (and (typep start 'fixnum)
              (typep end 'fixnum))
         'fixnum
         'integer))))

;;; Instruction Blueprints

(defmethod blueprint ((call-instruction call-instruction))
  (ucons:ulist* :call
                (operator call-instruction)
                (ucons:umapcar #'blueprint-from-value
                               (arguments call-instruction))))

(defmethod blueprint ((load-instruction load-instruction))
  (ucons:ulist* :load
                (buffer-number (buffer load-instruction))
                (blueprint (transformation load-instruction))))

(defmethod blueprint ((store-instruction store-instruction))
  (ucons:ulist* :store
                (blueprint-from-value (value store-instruction))
                (buffer-number (buffer store-instruction))
                (blueprint (transformation store-instruction))))

(defmethod blueprint ((iref-instruction iref-instruction))
  (let ((axis (axis iref-instruction)))
    (block nil
      (map-transformation-outputs
       (transformation iref-instruction)
       (lambda (output-index input-index scaling offset)
         (when (= output-index axis)
           (return
             (ucons:ulist 'range input-index scaling offset))))))))

(defmethod blueprint ((reduce-instruction reduce-instruction))
  (ucons:ulist* :reduce
                (operator reduce-instruction)
                (ucons:umapcar #'blueprint-from-value
                               (arguments reduce-instruction))))

(defmethod blueprint ((transformation transformation))
  (let ((result '()))
    (map-transformation-outputs
     transformation
     (lambda (output-index input-index scaling offset)
       (declare (ignore output-index))
       (setf result (ucons:ucons (ucons:ulist input-index scaling offset) result)))
     :from-end t)
    result))

(defun blueprint-from-value (value)
  (destructuring-bind (value-n . instruction) value
    (ucons:ulist value-n (instruction-number instruction))))

(defun buffer-number (buffer)
  (position buffer *buffers*))
