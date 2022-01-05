;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.scheduler)

;;; A task consists of a single kernel, together with a range of workers
;;; operating on it.  The scheduler turns each slice into a list of tasks
;;; before submitting it to the individual workers.  Each worker can then
;;; efficiently determine whether it is involved with the task and what its
;;; role is.

(defstruct (task
            (:predicate taskp)
            (:constructor make-task (kernel workers)))
  (kernel nil :type petalisp.ir:kernel)
  (workers nil :type range))

;;; The next function turns a slice into a list of tasks.  This
;;; distributions of workers to tasks should be optimal in terms of
;;; required energy to solution.
;;;
;;; The required energy consists of the following parts:
;;;
;;; 1. Base Load.  The idle power consumption of each worker, times the number of
;;;    workers, times the predicted execution time of the slice.  The
;;;    difficulty of this part is that the execution time may be hard to
;;;    predict.
;;;
;;; 2. Churn.  The additional power that a kernel requires to evaluate an
;;;    assigned work item, summed over all kernels and work items.
;;;    Computing this metric has two challenges.  For one, the parallel
;;;    efficiency depends on how many workers collaboratively work on one
;;;    kernel.  If the number is too low, it results in poor utilization of
;;;    memory controllers and caches.  If the number is too high, the
;;;    synchronization overhead and contention for resources might
;;;    dominate.  The second challenge arises when a kernel contains calls
;;;    to unknown functions.  In that case, we can only give a lower bound
;;;    on its cost - the cost of calling that function.
;;;
;;; 3. Kernel Invocation.  There is a small, but non-negligible cost
;;;    involved in calling a kernel.
;;;
;;; Finding a good solution to this problem is a daunting task.  Instead,
;;; we assume that the energy to solution for computing a kernel is
;;; independent of the number of workers, and that the cost of kernel
;;; invocation is negligible.  With thee assumptions in place, the energy
;;; to solution is now only a function of the total execution time.
;;;
;;; Our final assumption is that within a kernel, each worker will perform
;;; the same amount of work, i.e., we can actually represent the execution
;;; of a kernel with just its kernels and the worker ids.
;;;
;;; The algorithm we use is as follows:
;;;
;;; 1. We identify the set of 'expensive kernels', i.e., all kernels that
;;;    are costly enough to utilize all workers.  These kernels are
;;;    converted to tasks one by one.
;;;
;;; 2. For the remaining kernels, we know that they can keep only a
;;;    fraction of the work force busy.  What we do now is that we sort the
;;;    kernels by cost, and split them in the middle.  We compute the cost
;;;    of each part, and divide the workforce appropriately.  Now we repeat
;;;    step 1 for both parts and the corresponding workforce.

(defun tasks-from-slice (slice workers)
  (let ((kernels (sort (coerce (slice-kernels slice) 'simple-vector) #'>
                       :key #'petalisp.ir:kernel-cost)))
    (labels ((tasks-from-kernels (workers start end tasks)
               (if (= (range-size workers) 1)
                     (loop for index from start below end do
                       (push (make-task (svref kernels index) workers)
                             tasks)
                           finally (return tasks))
                     (let ((threshold (* (range-size workers) 1000)))
                       (peel-expensive-kernels workers threshold start end tasks))))
             (peel-expensive-kernels (workers threshold start end tasks)
               (if (= start end)
                   tasks
                   (let ((kernel (svref kernels start)))
                     (if (> (petalisp.ir:kernel-cost kernel) threshold)
                         (peel-expensive-kernels
                          workers
                          threshold
                          (1+ start)
                          end
                          (cons (make-task kernel workers) tasks))
                         (divide-and-conquer workers start end tasks)))))
             (divide-and-conquer (workers start end tasks)
               (let ((middle (+ start (floor (- end start) 2))))
                 (multiple-value-bind (left-workers right-workers)
                     (split-workers
                      workers
                      (reduce #'+ kernels
                              :key #'petalisp.ir:kernel-cost
                              :start start
                              :end middle)
                      (reduce #'+ kernels
                              :key #'petalisp.ir:kernel-cost
                              :start middle
                              :end end))
                   (nconc
                    (tasks-from-kernels left-workers start middle '())
                    (tasks-from-kernels right-workers middle end '())
                    tasks)))))
      (nreverse
       (tasks-from-kernels workers 0 (length kernels) '())))))

;;; Split WORKERS in two halves, such that the ratio of the two new worker
;;; pools is roughly equal to the ratio of COST-1 and COST-2, and that no
;;; worker pool is empty.
(defun split-workers (workers cost-1 cost-2)
  (let ((start (range-start workers))
        (size (range-size workers))
        (end (range-end workers)))
    (assert (< 1 size))
    (let* ((size-1 (floor (* size cost-1)  (+ cost-1 cost-2)))
           (size-2 (- size size-1)))
      (when (zerop size-1)
        (decf size-2)
        (incf size-1))
      (values
       (range start (+ start size-1))
       (range (+ start size-1) end)))))
