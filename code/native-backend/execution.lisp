;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defmethod compute-immediates
    ((lazy-arrays list) (native-backend native-backend))
  (let ((memory-pool (memory-pool native-backend)))
    (petalisp.scheduler:schedule-on-workers
     lazy-arrays
     (worker-count native-backend)
     ;; Execute.
     (lambda (tasks)
       (loop for task in tasks do
         (let ((kernel (petalisp.scheduler:task-kernel task)))
           (compile-and-execute-kernel kernel native-backend))))
     ;; Allocate.
     (lambda (buffer)
       (setf (buffer-storage buffer)
             (memory-pool-allocate
              memory-pool
              (petalisp.type-inference:type-specifier
               (buffer-ntype buffer))
              (mapcar #'range-size (shape-ranges (buffer-shape buffer))))))
     ;; Deallocate.
     (lambda (buffer)
       (let ((storage (buffer-storage buffer)))
         (unless (null storage)
           (setf (buffer-storage buffer) nil)
           (when (buffer-reusablep buffer)
             (memory-pool-free memory-pool storage))))))))

;; Cleanup.
(defmethod compute-immediates :after
    ((lazy-arrays list) (native-backend native-backend))
  (memory-pool-reset (memory-pool native-backend)))

(defun kernel-ranges (kernel)
  (let* ((iteration-space (kernel-iteration-space kernel))
         (rank (shape-rank iteration-space))
         (vector (make-array (* 3 rank))))
    (loop for index below rank
          for range in (shape-ranges iteration-space) do
      (multiple-value-bind (start step end)
          (range-start-step-end range)
        (setf (svref vector (+ (* 3 index) 0)) start)
        (setf (svref vector (+ (* 3 index) 1)) step)
        (setf (svref vector (+ (* 3 index) 2)) end)))
    vector))

(defun kernel-functions (kernel size)
  (let ((vector (make-array size))
        (current 0))
    (flet ((register-function (function)
             ;; If FUNCTION is a symbol, it is part of the kernel blueprint
             ;; and we don't need to pass it explicitly.
             (unless (symbolp function)
               (unless (find function vector :test #'eq :end current)
                 (cond ((= current size)
                        (return-from kernel-functions
                          (kernel-functions kernel (* 5 size))))
                       (t
                        (setf (svref vector current) function)
                        (incf current)))))))
      (map-instructions
       (lambda (instruction)
         (cond ((call-instruction-p instruction)
                (register-function (call-instruction-operator instruction)))
               ((reduce-instruction-p instruction)
                (register-function (reduce-instruction-operator instruction)))))
       kernel)
      vector)))

(defun compile-and-execute-kernel (kernel backend)
  (let ((ranges (kernel-ranges kernel))
        (arrays (map 'vector #'buffer-storage (kernel-buffers kernel)))
        (functions (kernel-functions kernel 8))
        (compiled-kernel
          (let ((blueprint (kernel-blueprint kernel)))
            (petalisp.utilities:with-hash-table-memoization (blueprint)
                (compile-cache backend)
              (compile nil (lambda-expression-from-blueprint blueprint))))))
    (assert (notany #'null arrays))
    ;; Now call the compiled kernel.
    (funcall compiled-kernel ranges arrays functions)))
