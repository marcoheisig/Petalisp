;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defmethod compute-immediates
    ((lazy-arrays list) (native-backend native-backend))
  (let ((memory-pool (memory-pool native-backend))
        (worker-pool (worker-pool native-backend)))
    (petalisp.scheduler:schedule-on-workers
     lazy-arrays
     (worker-pool-size worker-pool)
     ;; Execute.
     (lambda (tasks)
       (loop for task in tasks do
         (let* ((kernel (petalisp.scheduler:task-kernel task))
                (workers (petalisp.scheduler:task-workers task))
                (fn (compile-kernel kernel native-backend)))
           (worker-pool-enqueue
            (lambda (worker-id)
              (invoke-kernel kernel fn workers worker-id))
            worker-pool)))
       (worker-pool-wait worker-pool))
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

(defun kernel-ranges (kernel n-workers normalized-worker-id)
  (assert (< normalized-worker-id n-workers))
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
    ;; So far, we use a simple outer loop parallelization scheme.  To do
    ;; that, we first have to check whether there is an outer loop.
    (if (= rank 1)
        ;; If there is no outer loop, we execute the kernel on one
        ;; worker only.
        (when (zerop normalized-worker-id)
          vector)
        (symbol-macrolet ((start (svref vector 3))
                          (step (svref vector 4))
                          (end (svref vector 5)))
          (let ((outer-loop-size (/ (1+ (- end start)) step)))
            (multiple-value-bind (chunk-size remainder)
                (floor outer-loop-size n-workers)
              (if (zerop chunk-size)
                  (when (< normalized-worker-id remainder)
                    (setf start (+ start (* step normalized-worker-id)))
                    (setf end start)
                    vector)
                  (let* ((new-start (+ start
                                       (* normalized-worker-id chunk-size step)
                                       (* (min remainder normalized-worker-id) step)))
                         (new-end (+ new-start
                                     (* (1- chunk-size) step)
                                     (if (< normalized-worker-id remainder) step 0))))
                    (setf start new-start)
                    (setf end new-end)
                    vector))))))))

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

(defun invoke-kernel (kernel kernel-fn workers worker-id)
  (when (range-contains workers worker-id)
    (let* ((base-id (range-start workers))
           (ranges (kernel-ranges kernel (range-size workers) (- worker-id base-id)))
           (arrays (map 'vector #'buffer-storage (kernel-buffers kernel)))
           (functions (kernel-functions kernel 8)))
      (when ranges
        (funcall kernel-fn ranges arrays functions)))))

(defun compile-kernel (kernel backend)
  (let ((blueprint (kernel-blueprint kernel)))
    (petalisp.utilities:with-hash-table-memoization (blueprint)
        (compile-cache backend)
      (compile nil (lambda-expression-from-blueprint blueprint)))))
