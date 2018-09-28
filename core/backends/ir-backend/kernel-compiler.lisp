;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Closure Compiler for Kernels

(defun make-load-thunk (reference)
  ;; TODO
  )

(defun make-store-thunk (reference)
  ;; TODO
  )

(defun compile-kernel (kernel)
  ;; TODO
  )

(defun compile-outer-loops (ranges body-fn)
  (if (null ranges)
      body-fn
      (let ((body-fn (compile-outer-loops (cdr ranges) body-fn)))
        (multiple-value-bind (start step end)
            (range-start-step-end (car ranges))
          (lambda (index)
            (loop for i from start by step to end do
              (funcall body-fn (cons i index))))))))

(defun compile-kernel-body (body)
  ;; TODO
  )
