;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-task-queue global-evaluator-thread)

(defun evaluate-naively (vm immediate)
  ;; only evaluate once
  (unless (storage immediate)
    ;; evaluate all dependencies
    (let (dependencies)
      (iterate
        (for kernel in (kernels immediate))
        (iterate
          (for source in-vector (sources kernel))
          (pushnew source dependencies)))
      (map nil (λ dependency (evaluate-naively vm dependency)) dependencies))
    ;; allocate memory
    (vm/bind-memory vm immediate)
    ;; compute all kernels
    (iterate
      (for kernel in (kernels immediate))
      (vm/execute vm kernel)
      ;; potentially release resources
      (iterate
        (for source in-vector (sources kernel))
        (when (zerop (decf (refcount source)))
          (vm/free-memory vm source))))
    immediate))
