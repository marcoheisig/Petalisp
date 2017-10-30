;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-task-queue global-evaluator-thread)

(defun schedule-asynchronously (virtual-machine data-structures)
  (assert (every #'strided-array? data-structures))
  (let* ((graph-roots (map 'vector #'shallow-copy data-structures))
         (targets (map 'vector (λ x (change-class x 'strided-array-immediate)) data-structures))
         (request (make-request)))
    ;; TODO currently schedules synchronously for easier debugging
    (%schedule virtual-machine targets graph-roots request)
    #+nil
    (prog1 request
      (run-in-global-evaluator-thread
       (λ (%schedule virtual-machine targets blueprints request))))))

(defgeneric evaluate (object))

(defun %schedule (virtual-machine targets graph-roots request)
  (let ((*virtual-machine* virtual-machine))
    (iterate (for immediate in-sequence (kernelize-graph graph-roots))
             (for index from 0)
             (setf (storage (aref targets index))
                   (storage (evaluate immediate))))
    (complete request)))

(defmethod evaluate ((immediate immediate))
  ;; only evaluate once
  (unless (storage immediate)
    ;; evaluate all dependencies
    (let (dependencies)
      (iterate
        (for kernel in (kernels immediate))
        (iterate
          (for source in-vector (sources kernel))
          (pushnew source dependencies)))
      (map nil #'evaluate (dependencies immediate)))
    ;; allocate memory
    (vm/bind-memory immediate)
    ;; compute all kernels
    (iterate
      (for kernel in (kernels immediate))
      (vm/execute kernel)
      ;; potentially release resources
      (iterate
        (for source in-vector (sources kernel))
        (when (zerop (decf (refcount source)))
          (vm/free-memory source))))
    immediate))
