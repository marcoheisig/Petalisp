;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir-backend)

(defgeneric execute (ir-node))

(defmethod execute ((ir-node ir-node))
  (values))

(defmethod execute :around ((ir-node ir-node))
  (unless (executedp ir-node)
    (setf (executedp ir-node) t)
    (call-next-method)))

(defmethod execute :before ((kernel kernel))
  (mapc (compose #'execute #'petalisp.ir:buffer)
        (petalisp.ir:loads kernel)))

(defmethod execute :before ((buffer buffer))
  (mapc #'execute (inputs buffer)))

(defmethod compute-immediates ((strided-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (petalisp.ir:ir-from-strided-arrays strided-arrays ir-backend)))
    (petalisp.ir:normalize-ir root-buffers)
    (mapc #'execute root-buffers)
    (mapcar #'immediate-from-buffer root-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execution of Instructions

;;; The current kernel.
(defvar *kernel*)

;;; The current loop index.
(defvar *index*)

;;; The generic function INSTRUCTION-VALUES returns the values of the given
;;; instruction, as a list.  Some instructions, e.g., loads, are influenced
;;; by the special variable *INDEX*.
(defgeneric instruction-values (instruction))

(defmethod instruction-values ((call-instruction petalisp.ir:call-instruction))
  (multiple-value-list
   (apply (petalisp.ir:operator call-instruction)
          (loop for (value-n . instruction)
                  in (petalisp.ir:arguments call-instruction)
                collect (nth value-n (instruction-values instruction))))))

(defmethod instruction-values ((load-instruction petalisp.ir:load-instruction))
  (list
   (bref (petalisp.ir:buffer load-instruction)
         (transform *index* (petalisp.ir:transformation load-instruction)))))

(defmethod instruction-values ((store-instruction petalisp.ir:store-instruction))
  (setf (bref (petalisp.ir:buffer store-instruction)
              (transform *index* (petalisp.ir:transformation store-instruction)))
        (destructuring-bind (value-n . instruction)
            (petalisp.ir:value store-instruction)
          (nth value-n (instruction-values instruction))))
  (list))

(defmethod instruction-values ((reduce-instruction petalisp.ir:reduce-instruction))
  (let ((k (length (petalisp.ir:arguments reduce-instruction))))
    (labels ((divide-and-conquer (range)
               (if (size-one-range-p range)
                   (let ((*index* (cons (range-start range) *index*)))
                     ;; *index* is changed, so we also have to clear the cache.
                     (clear-instruction-values-cache)
                     (loop for (value-n . instruction)
                             in (petalisp.ir:arguments reduce-instruction)
                           collect (nth value-n (instruction-values instruction))))
                   (multiple-value-bind (left right)
                       (split-range range)
                     (subseq
                      (multiple-value-list
                       (multiple-value-call (operator reduce-instruction)
                         (values-list (divide-and-conquer left))
                         (values-list (divide-and-conquer right))))
                      0 k)))))
      (divide-and-conquer (reduction-range *kernel*)))))

(defmethod instruction-values ((iref-instruction petalisp.ir:iref-instruction))
  (transform *index* (petalisp.ir:transformation iref-instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Value Caching

;;; An array, mapping from instruction numbers to instruction values.
(defvar *instruction-values-cache*)

(defun instruction-values-cache (instruction)
  (aref *instruction-values-cache* (petalisp.ir:instruction-number instruction)))

(defun (setf instruction-values-cache) (value instruction)
  (setf (aref *instruction-values-cache* (petalisp.ir:instruction-number instruction))
        value))

(defun clear-instruction-values-cache ()
  (fill *instruction-values-cache* 0))

(defmethod instruction-values :around
    ((instruction petalisp.ir:instruction))
  (let ((cache (instruction-values-cache instruction)))
    (if (listp cache)
        cache
        (setf (instruction-values-cache instruction)
              (call-next-method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Execution

(defmethod execute ((kernel petalisp.ir:kernel))
  (let* ((n-instructions (1+ (petalisp.ir:highest-instruction-number kernel)))
         (*instruction-values-cache* (make-array n-instructions))
         (*kernel* kernel))
    (set-for-each
     (lambda (index)
       (let ((*index* index))
         ;; Clear the cached values of the previous iteration.
         (clear-instruction-values-cache)
         ;; Evaluate all instructions with side-effects (= store
         ;; instructions) and their dependencies.
         (mapc #'instruction-values (petalisp.ir:stores kernel))))
     (petalisp.ir:iteration-space kernel))))
