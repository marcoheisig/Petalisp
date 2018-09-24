;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Closure Compiler for Kernels
;;;
;;; We convert each statement of the kernel body, as well as each
;;; surrounding loop, into a function of the surrounding index.

(defun compile-kernel (kernel)
  (let* ((storages (mapcar #'storage (petalisp-ir:outputs kernel)))
         ;; 1. Compile the body.
         (body-fn (compile-kernel-body (petalisp-ir:body kernel)))
         ;; 2. Wrap it, such that the results are written to the outputs.
         (body-fn (lambda (index)
                    (loop for storage in storages
                          for value in (multiple-value-list
                                        (funcall body-fn index))
                          do (setf (apply #'aref storage index) value))))
         ;; 3. Wrap this expression in a series of loops.
         (body-fn (compile-outer-loops
                   (reverse (ranges (shape kernel)))
                   body-fn)))
    (lambda ()
      (funcall body-fn '()))))

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
  (trivia:ematch body
    ;; Read
    ((list 'petalisp-ir:pref buffer transformation)
     (let ((storage (storage buffer))
           (transformation
             (compose-transformations
              (transformation buffer)
              transformation)))
       (lambda (index)
         (apply #'aref storage (transform index transformation)))))
    ;; Reduce
    ((list* 'petalisp-ir:preduce size value-n operator arguments)
     (let ((arg-fns (mapcar #'compile-kernel-body arguments)))
       (lambda (index)
         (labels ((divide-and-conquer (imin imax)
                    (if (= imin imax)
                        (let* ((new-index (cons imin index)))
                          (values-list
                           (loop for arg-fn in arg-fns
                                 collect
                                 (funcall arg-fn new-index))))
                        (let ((middle (floor (+ imin imax) 2)))
                          (multiple-value-call operator
                            (divide-and-conquer imin middle)
                            (divide-and-conquer (1+ middle) imax))))))
           (nth-value value-n (divide-and-conquer 0 (1- size)))))))
    ;; Call
    ((list* 'petalisp-ir:pcall value-n operator arguments)
     (let ((arg-fns (mapcar #'compile-kernel-body arguments)))
       (lambda (index)
         (let ((args (loop for arg-fn in arg-fns
                           collect
                           (funcall arg-fn index))))
           (nth-value value-n (apply operator args))))))))
