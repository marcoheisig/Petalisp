;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; In this file we define a simple kernel interpreter.  The interpreter
;;; can be used for testing or for executing small kernels for which
;;; compilation is not worth it.

(defun interpret-kernel (kernel iteration-space)
  (let* ((instruction-vector (kernel-instruction-vector kernel))
         (offset-vector (make-array (length instruction-vector)))
         (value-vector-length 0))
    (declare (simple-vector instruction-vector offset-vector))
    ;; Compute the number of entries in the value vector, and have the
    ;; entry of the offset vector of each instruction point to the correct
    ;; place in the value vector.
    (loop for instruction across instruction-vector
          for instruction-number from 0
          for number-of-values = (instruction-number-of-values instruction)
          do (assert (= instruction-number (instruction-number instruction)))
          do (setf (svref offset-vector instruction-number)
                   value-vector-length)
          do (incf value-vector-length number-of-values))
    ;; Introduce the value vector and functions working on it.
    (let ((value-vector (make-array value-vector-length)))
      (declare (simple-vector value-vector))
      (labels
          ((interpret-instruction (instruction offset index)
             (etypecase instruction
               (call-instruction
                (with-accessors ((number-of-values call-instruction-number-of-values)
                                 (operator call-instruction-operator)
                                 (inputs call-instruction-inputs)) instruction
                  (case number-of-values
                    (0 (values))
                    (1 (setf (svref value-vector offset)
                             (apply operator (mapcar #'input-value inputs))))
                    (otherwise
                     (let ((values (multiple-value-list
                                    (apply operator (mapcar #'input-value inputs)))))
                       (loop for index from 0 below number-of-values
                             do (setf (svref value-vector (+ offset index))
                                      (pop values))))))))
               (iref-instruction
                (with-accessors ((transformation iref-instruction-transformation)) instruction
                  (setf (svref value-vector offset)
                        (first
                         (transform index transformation)))))
               (load-instruction
                (with-accessors ((transformation load-instruction-transformation)
                                 (buffer load-instruction-buffer)) instruction
                  (let ((array (buffer-storage buffer)))
                    (setf (svref value-vector offset)
                          (apply #'aref array (transform index transformation))))))
               (store-instruction
                (with-accessors ((transformation store-instruction-transformation)
                                 (buffer store-instruction-buffer)
                                 (input store-instruction-input)) instruction
                  (let ((array (buffer-storage buffer)))
                    (setf (apply #'aref array (transform index transformation))
                          (input-value input)))))))
           (input-value (input)
             (destructuring-bind (value-n . instruction) input
               (let ((offset (svref offset-vector (instruction-number instruction))))
                 (svref value-vector (+ offset value-n))))))
        ;; Execute the instructions once for each point of the iteration
        ;; space.
        (map-shape
         (lambda (index)
           (loop for instruction across instruction-vector
                 for offset across offset-vector
                 do (interpret-instruction instruction offset index)))
         iteration-space)))))
