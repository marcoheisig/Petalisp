;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Closure Compiler for Kernels
;;;
;;; We convert each statement of the kernel body, as well as each
;;; surrounding loop, into a function of the surrounding index.

(defun compile-kernel (kernel)
  (let ((body-fn (compile-outer-loops
                  (reverse (ranges (shape kernel)))
                  (compile-kernel-body
                   (body kernel)))))
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
    ;; Write
    ((list 'pstore buffer form)
     (let ((storage (storage buffer))
           (transformation (transformation buffer))
           (body-fn (compile-kernel-body form)))
       (lambda (index)
         (setf (apply #'aref storage (transform index transformation))
               (funcall body-fn index)))))
    ;; Read
    ((list 'pref buffer transformation)
     (let ((storage (storage buffer))
           (transformation
             (compose-transformations
              (transformation buffer)
              transformation)))
       (lambda (index)
         (apply #'aref storage (transform index transformation)))))
    ;; Reduce
    ((list* 'preduce range value-n operator arguments)
     (let ((arg-fns (mapcar #'compile-kernel-body arguments))
           (size (set-size range)))
       (multiple-value-bind (start step end)
           (range-start-step-end range)
         (declare (ignore end))
         (lambda (index)
           (labels ((divide-and-conquer (imin imax)
                      (if (= imin imax)
                          (let* ((new-index (cons (+ start (* imin step)) index)))
                            (values-list
                             (loop for arg-fn in arg-fns
                                   collect
                                   (funcall arg-fn new-index))))
                          (let ((middle (floor (+ imin imax) 2)))
                            (multiple-value-call operator
                              (divide-and-conquer imin middle)
                              (divide-and-conquer (1+ middle) imax))))))
             (nth-value value-n (divide-and-conquer 0 (1- size))))))))
    ;; Call
    ((list* 'pcall value-n operator arguments)
     (let ((arg-fns (mapcar #'compile-kernel-body arguments)))
       (lambda (index)
         (let ((args (loop for arg-fn in arg-fns
                           collect
                           (funcall arg-fn index))))
           (nth-value value-n (apply operator args))))))))
