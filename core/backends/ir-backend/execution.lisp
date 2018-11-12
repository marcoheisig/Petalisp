;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

(defgeneric execute (ir-node))

(defmethod execute ((ir-node ir-node))
  (values))

(defmethod execute :around ((ir-node ir-node))
  (unless (executedp ir-node)
    (setf (executedp ir-node) t)
    (call-next-method)))

(defmethod execute :before ((kernel kernel))
  (mapc (compose #'execute #'petalisp-ir:buffer)
        (petalisp-ir:loads kernel)))

(defmethod execute :before ((buffer buffer))
  (mapc #'execute (inputs buffer)))

(defmethod compute-immediates ((strided-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (petalisp-ir:ir-from-strided-arrays strided-arrays ir-backend)))
    (petalisp-ir:normalize-ir root-buffers)
    (mapc #'execute root-buffers)
    (mapcar #'immediate-from-buffer root-buffers)))

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
   (bref (petalisp-ir:buffer load-instruction)
         (transform *index* (petalisp-ir:transformation load-instruction)))))

(defmethod instruction-values
    ((store-instruction petalisp-ir:store-instruction))
  (setf (bref (petalisp-ir:buffer store-instruction)
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
  (transform *index* (petalisp-ir:transformation iref-instruction)))

(defmethod execute ((kernel petalisp-ir:kernel))
  (let* ((size (1+ (petalisp-ir:highest-instruction-number kernel)))
         (*instruction-values* (make-array size)))
    (if (not (petalisp-ir:reduction-kernel-p kernel))
        ;; Non-reduction kernels.
        (set-for-each
         (lambda (index)
           (let ((*index* index))
             (fill *instruction-values* 0) ; Clear the previous values
             (mapc #'instruction-values (petalisp-ir:stores kernel))))
         (petalisp-ir:iteration-space kernel))
        ;; Reduction kernels.
        (let* ((ranges (ranges (petalisp-ir:iteration-space kernel)))
               (reduction-range (first ranges))
               (non-reducing-iteration-space (apply #'make-shape (rest ranges)))
               (reduce-instructions (petalisp-ir:kernel-reduce-instructions kernel)))
          (set-for-each
           (lambda (non-reducing-index)
             (labels ((divide-and-conquer (range)
                        (if (size-one-range-p range)
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
                                     0
                                     (length (petalisp-ir:arguments reduce-instruction))))))))
               (loop for reduce-instruction in reduce-instructions
                     for values in (divide-and-conquer reduction-range) do
                       (setf (instruction-values reduce-instruction) values))
               (let ((*index* non-reducing-index))
                 (loop for reduction-store in (petalisp-ir:reduction-stores kernel) do
                   (instruction-values reduction-store)))))
           non-reducing-iteration-space)))))
