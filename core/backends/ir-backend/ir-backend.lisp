;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;; The purpose of the IR backend is to check that the IR conversion
;;; preserves semantics.  It is similar to the reference backend, but
;;; evaluates kernels instead of individual strided arrays.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric execute (ir-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-backend (petalisp:backend)
  ())

(defclass ir-backend-buffer (petalisp-ir:buffer)
  ((%storage :initarg :storage :reader storage)
   (%executedp :initarg :executedp :accessor executedp)))

(defun make-ir-backend ()
  (make-instance 'ir-backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-immediates ((strided-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (petalisp-ir:ir-from-strided-arrays strided-arrays ir-backend)))
    (petalisp-ir:normalize-ir root-buffers)
    (mapc #'execute root-buffers)
    (mapcar (compose #'make-array-immediate #'storage) root-buffers)))

(defmethod execute :before ((kernel petalisp-ir:kernel))
  (mapc (compose #'execute #'petalisp-ir:buffer)
        (petalisp-ir:loads kernel)))

(defmethod execute :around ((ir-backend-buffer ir-backend-buffer))
  (unless (executedp ir-backend-buffer)
    (setf (executedp ir-backend-buffer) t)
    (call-next-method)))

(defmethod execute ((ir-backend-buffer ir-backend-buffer))
  (mapc #'execute (inputs ir-backend-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execution of Instructions

;;; An array, mapping from instruction numbers to instruction values.
(defvar *instruction-values*)

;;; The current loop index.
(defvar *index*)

(defun (setf instruction-values) (value instruction)
  (setf (aref *instruction-values* (petalisp-ir:instruction-number instruction))
        value))

(defgeneric instruction-values (instruction))

(defmethod instruction-values :around
    ((instruction petalisp-ir:instruction))
  (let* ((n (petalisp-ir:instruction-number instruction)))
    (if (eql (aref *instruction-values* n) 0)
        (setf (aref *instruction-values* n) (call-next-method))
        (aref *instruction-values* n))))

(defmethod instruction-values
    ((call-instruction petalisp-ir:call-instruction))
  (multiple-value-list
   (apply (petalisp-ir:operator call-instruction)
          (loop for (value-n . instruction)
                  in (petalisp-ir:arguments call-instruction)
                collect (nth value-n (instruction-values instruction))))))

(defmethod instruction-values
    ((load-instruction petalisp-ir:load-instruction))
  (list
   (apply #'aref (storage (petalisp-ir:buffer load-instruction))
          (transform *index* (petalisp-ir:transformation load-instruction)))))

(defmethod instruction-values
    ((store-instruction petalisp-ir:store-instruction))
  (setf (apply #'aref
               (storage (petalisp-ir:buffer store-instruction))
               (transform *index* (petalisp-ir:transformation store-instruction)))
        (destructuring-bind (value-n . instruction)
            (petalisp-ir:value store-instruction)
          (nth value-n (instruction-values instruction))))
  (list))

(defmethod instruction-values
    ((reduce-instruction petalisp-ir:reduce-instruction))
  (loop for (value-n . instruction)
          in (petalisp-ir:arguments reduce-instruction)
        collect (nth value-n (instruction-values instruction))))

(defmethod instruction-values
    ((iref-instruction petalisp-ir:iref-instruction))
  (list
   (nth (petalisp-ir:axis iref-instruction)
        (transform *index* (petalisp-ir:transformation iref-instruction)))))

(defmethod execute ((kernel petalisp-ir:kernel))
  (let* ((size (1+ (petalisp-ir:highest-instruction-number kernel)))
         (*instruction-values* (make-array size)))
    (if (not (petalisp-ir:reduction-kernel-p kernel))
        ;; Non-reduction kernels.
        (loop for index in (set-elements (petalisp-ir:iteration-space kernel)) do
          (let ((*index* index))
            (fill *instruction-values* 0) ; Clear the previous values
            (mapc #'instruction-values (petalisp-ir:stores kernel))))
        ;; Reduction kernels.
        (let* ((ranges (ranges (petalisp-ir:iteration-space kernel)))
               (reduction-range (first ranges))
               (non-reducing-iteration-space (shape-from-ranges (rest ranges)))
               (reduce-instructions (petalisp-ir:kernel-reduce-instructions kernel)))
          (loop for non-reducing-index in (set-elements non-reducing-iteration-space) do
            (labels ((divide-and-conquer (range)
                       (if (unary-range-p range)
                           (let ((*index* (cons (range-start range) non-reducing-index)))
                             (fill *instruction-values* 0)
                             (mapc #'instruction-values (petalisp-ir:stores kernel))
                             (mapcar #'instruction-values reduce-instructions))
                           (multiple-value-bind (left right)
                               (split-range range)
                             (loop for left-values in (divide-and-conquer left)
                                   for right-values in (divide-and-conquer right)
                                   for reduce-instruction in reduce-instructions
                                   collect
                                   (subseq
                                    (multiple-value-list
                                     (multiple-value-call (operator reduce-instruction)
                                       (values-list left-values)
                                       (values-list right-values)))
                                    0 (length (petalisp-ir:arguments reduce-instruction))))))))
              (loop for reduce-instruction in reduce-instructions
                    for values in (divide-and-conquer reduction-range) do
                      (setf (instruction-values reduce-instruction) values))
              (let ((*index* non-reducing-index))
                (loop for reduction-store in (petalisp-ir:reduction-stores kernel) do
                  (instruction-values reduction-store)))))))))
