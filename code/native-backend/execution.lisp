;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defmethod backend-compute
    ((native-backend native-backend)
     (lazy-arrays list))
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
                (fn (compile-kernel-on-backend kernel native-backend)))
           (worker-pool-enqueue
            (lambda (worker-id)
              (invoke-kernel kernel fn workers worker-id))
            worker-pool))))
     ;; Barrier.
     (lambda ()
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
           (when (petalisp.ir:interior-buffer-p buffer)
             (memory-pool-free memory-pool storage))))))))

;; Cleanup.
(defmethod backend-compute :after
    ((native-backend native-backend)
     (lazy-arrays list))
  (memory-pool-reset (memory-pool native-backend)))

(defun compile-kernel-on-backend (kernel backend)
  (let ((blueprint (kernel-blueprint kernel)))
    (petalisp.utilities:with-hash-table-memoization (blueprint)
        (compile-cache backend)
      (compile nil (petalisp.ir:translate-blueprint blueprint)))))

(defun invoke-kernel (kernel kernel-fn workers worker-id)
  (when (range-contains workers worker-id)
    (let* ((base-id (range-start workers))
           (iteration-space
             (worker-iteration-space kernel (range-size workers) (- worker-id base-id))))
      (unless (not iteration-space)
        (funcall kernel-fn kernel iteration-space)))))

(defun worker-iteration-space (kernel n-workers normalized-worker-id)
  (assert (< normalized-worker-id n-workers))
  (let* ((iteration-space (kernel-iteration-space kernel))
         (ranges (shape-ranges iteration-space)))
    (flet ((use-serial-execution ()
             (return-from worker-iteration-space
               (if (zerop normalized-worker-id)
                   iteration-space
                   nil))))
      ;; If the kernel has rank zero, we execute it only on worker zero.
      (when (null ranges)
        (use-serial-execution))
      ;; If the kernel's outer loop is too small, we execute it only on
      ;; worker zero.
      (when (< (range-size (first ranges)) n-workers)
        (use-serial-execution))
      ;; We use a simple outer loop parallelization scheme for now.
      (make-shape
       (list*
        (let* ((range (first ranges))
               (start (range-start range))
               (step (range-step range))
               (outer-loop-size (range-size range)))
          (multiple-value-bind (chunk-size remainder)
              (floor outer-loop-size n-workers)
            (let* ((new-start
                     (+ start
                        (* normalized-worker-id chunk-size step)
                        (* (min remainder normalized-worker-id) step)))
                   (new-end
                     (+ new-start
                        (* chunk-size step)
                        (if (< normalized-worker-id remainder) step 0))))
              (range new-start new-end step))))
        (rest ranges))))))
