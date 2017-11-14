;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class default-scheduler-mixin ()
  ((scheduler-queue :type queue :initform (make-queue))
   (scheduler-thread :initform nil)))

(defmethod vm/schedule ((vm default-scheduler-mixin) graph-roots)
  (let* ((target-graphs
           )
         (targets
           )
         (request (make-request)))
    ;; TODO currently schedules synchronously for easier debugging
    (iterate (for immediate in-sequence (kernelize-graph target-graphs))
             (for index from 0)
             (setf (storage (aref targets index))
                   (storage (evaluate-naively vm immediate))))
    (complete request)
    #+nil
    (prog1 request
      (run-in-global-evaluator-thread
       (λ (%schedule virtual-machine targets blueprints request))))))

(defun evaluate-naively (vm immediate)
  ;; only evaluate once
  (unless (storage immediate)
    ;; evaluate all dependencies
    (let (dependencies)
      (iterate
        (for kernel in-sequence (kernels immediate))
        (iterate
          (for source in-vector (sources kernel))
          (pushnew source dependencies)))
      (map nil (λ dependency (evaluate-naively vm dependency)) dependencies))
    ;; allocate memory
    (vm/bind-memory vm immediate)
    ;; compute all kernels
    (iterate
      (for kernel in-sequence (kernels immediate))
      (vm/execute vm kernel)
      ;; potentially release resources
      (iterate
        (for source in-vector (sources kernel))
        (when (zerop (decf (refcount source)))
          (vm/free-memory vm source))))
    immediate))
