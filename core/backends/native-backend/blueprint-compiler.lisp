;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defun translation-unit-from-blueprint (blueprint)
  (destructuring-bind (ranges arrays instructions)
      (ucons:copy-utree blueprint)
    (let ((n-ranges (length ranges))
          (n-arrays (length arrays))
          (n-calls 0)
          (reduction-p nil)
          ;; An array with one entry per instruction of the blueprint.
          (n-values (make-array (length instructions) :initial-element 0)))
      ;; Compute n-calls and n-values.
      (loop for instruction in instructions do
        (etypecase (car instruction)
          ((:call-next-function) (incf n-calls))
          ((:load :store :iref) (values))
          ((:call :reduce)
           (when (eq (car instruction) :reduce)
             (setf reduction-p t))
           (loop for (value-n . instruction-number) in (cddr instruction) do
             (maxf (aref n-values instruction-number) value-n)))))
      ;; Now generate code.
      (let ((form-builders (make-form-builders)))
        (flet ((add-auxiliary-instruction (instruction depth)
                 (petalisp-memoization:with-multiple-value-memoization
                     (instruction :test #'equal)
                   (values (gensym) depth))))
          ))
      )))

(defun make-form-builders (dimension reduction-p)
  (let ((result (make-array (1+ dimension))))
    ()
    ))

(defun compile-blueprint (blueprint)
  (let ((lambda-expression
          (lambda-expression-from-translation-unit
           (translation-unit-from-blueprint blueprint))))
    (compile nil lambda-expression)))
