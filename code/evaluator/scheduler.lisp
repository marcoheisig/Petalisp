;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-task-queue global-evaluator-thread)

(defun schedule-asynchronously (data-structures)
  (assert (every #'strided-array? data-structures))
  (let* ((recipes (map 'vector #'shallow-copy data-structures))
         (targets (map 'vector (λ x (change-class x 'strided-array-immediate)) data-structures))
         (request (make-request)))
    (run-in-global-evaluator-thread
     (λ
      (labels ((evaluate (strided-array-immediate)
                 (iterate
                   (for kernel in (kernels strided-array-immediate))
                   (iterate (for source in-vector (sources kernel))
                            (when (and (strided-array-immediate? source)
                                       (not (storage source)))
                              (evaluate source))))
                 (evaluate strided-array-immediate)))
        (iterate (for item in-sequence (kernelize-graph recipes))
                 (for index from 0)
                 (setf (storage (aref targets index))
                       (storage (evaluate item))))
        (complete request))))
    request))

