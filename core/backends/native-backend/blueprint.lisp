;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; A blueprint is an s-expression made of ucons cells.  It contains all
;;; the information necessary to compute an efficient evaluation function
;;; for this kernel.  The idea is that blueprints can be used to cache
;;; compiled evaluation functions.

(defgeneric blueprint (kernel))

(defvar *kernel*)

(defmethod blueprint :around ((kernel kernel))
  (let ((*kernel* kernel))
    (call-next-method)))

(defmethod blueprint ((kernel kernel))
  (ucons:ulist
   (ucons:umapcar #'blueprint-from-buffer (buffers kernel))
   (ucons:umapcar #'blueprint-from-reference (petalisp-ir:stores kernel))
   (ucons:umapcar #'blueprint-from-reference (petalisp-ir:loads kernel))))

(defmethod blueprint ((kernel kernel))
  (ucons:ulist*
   (ucons:umapcar #'blueprint-from-loop-range (ranges (petalisp-ir:iteration-space kernel)))
   (call-next-method)))

(defgeneric blueprint-from-buffer (buffer)
  (:method ((buffer buffer))
    (ucons:ulist 'simple-array (element-type buffer)))
  (:method ((scalar-buffer scalar-buffer))
    (element-type scalar-buffer)))

(defun blueprint-from-loop-range (range)
  (ucons:ulist* :loop (blueprint-from-range range)))

;;; Return an ulist with three elements:
;;;
;;; 1. A boolean, indicating whether all indices in this range are fixnums.
;;;
;;; 2. An inclusive lower bound on the size of the range.
;;;
;;; 3. An exclusive upper bound on the size of the range.
(defun blueprint-from-range (range)
  (let ((bits (integer-length (size range)))
        (start (range-start range))
        (end (range-end range)))
    (ucons:ulist
     (and (typep start 'fixnum)
          (typep end 'fixnum))
     (ash 1 (1- bits))
     (ash 1 bits))))

(defun blueprint-from-instruction (instruction)
  (ucons:ulist
   (ucons:umapcar #'blueprint-from-store (petalisp-ir:stores instruction))
   (petalisp-ir:operator instruction)
   (ucons:umapcar #'blueprint-from-load (petalisp-ir:loads instruction))))

(defun blueprint-from-transformation (transformation)
  (let ((result '()))
    (map-transformation-outputs
     transformation
     (lambda (output-index input-index scaling offset)
       (declare (ignore output-index))
       (setf result (ucons:ucons (ucons:ulist input-index scaling offset) result)))
     :from-end t)))

(defun blueprint-from-store (store)
  (if (symbolp store)
      store
      (position store (petalisp-ir:stores *kernel*))))

(defun blueprint-from-load (load)
  (if (symbolp load)
      load
      (position load (petalisp-ir:loads *kernel*))))

(defun blueprint-from-reference (reference)
  (blueprint-from-destructured-reference
   (car reference)
   (cdr reference)))

(defgeneric blueprint-from-destructured-reference (buffer transformation))

(defmethod blueprint-from-destructured-reference
    ((scalar-immediate scalar-immediate)
     (transformation transformation))
  'scalar)

(defmethod blueprint-from-destructured-reference
    ((buffer buffer) ; Works for array immediates or non-immediate buffers.
     (transformation transformation))
  (ucons:ulist*
   'storage-ref
   (position buffer (buffers *kernel*))
   (blueprint-from-transformation transformation)))

(defmethod blueprint-from-destructured-reference
    ((range-immediate range-immediate)
     (transformation transformation))
  (let ((axis (axis range-immediate)))
    (block nil
      (map-transformation-outputs
       transformation
       (lambda (output-index input-index scaling offset)
         (when (= output-index axis)
           (return
             (ucons:ulist 'range input-index scaling offset))))))))
