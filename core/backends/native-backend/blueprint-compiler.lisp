;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The kernel compiler

(defun translate-simple-kernel (s-expression)
  (destructuring-bind (ranges stores loads statements)
      s-expression
    `(lambda (kernel)
       (values))))

(defun translate-reduction-kernel (s-expression)
  (destructuring-bind (reduction-range reduction-stores operator
                       ranges stores loads statements)
      s-expression
      (break)
      `(lambda (kernel)
         (values))))

(defun compile-blueprint (blueprint)
  (let* ((s-expression (ucons:copy-utree blueprint))
         (thunk (ecase (first s-expression)
                  (simple-kernel (translate-simple-kernel (rest s-expression)))
                  (reduction-kernel (translate-reduction-kernel (rest s-expression))))))
    (compile nil thunk)))

(defun stride (array axis)
  (declare (simple-array array)
           (array-index axis))
  (let ((stride 1))
    (declare (positive-fixnum stride))
    (loop for index of-type fixnum
          from (1- (array-rank array))
          downto (1+ axis) do
            (setf stride (* (array-dimension array index) stride)))
    stride))

