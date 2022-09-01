;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

;;; An evaluator's cstate captures all state that is independent of a
;;; particular invocation of that evaluator.

(defstruct (cstate
            (:constructor %make-cstate))
  (backend (alexandria:required-argument :backend)
   :type starpu-backend
   :read-only t)
  (program (alexandria:required-argument :program)
   :type petalisp.ir:program
   :read-only t)
  (allocations (alexandria:required-argument :allocations)
   :type list
   :read-only t)
  ;; A list of buffers corresponding to the evaluator's unknowns.
  (argument-buffers (alexandria:required-argument :argument-buffers)
   :type list
   :read-only t)
  ;; A simple vector with one StarPU codelet per kernel in the program.
  ;; Kernels with the same blueprint share the same codelet.
  (kernel-codelet-vector (alexandria:required-argument :kernel-codelet-vector)
   :type simple-vector
   :read-only t))

(defun make-cstate (backend unknowns lazy-arrays)
  (let* ((root-buffers (ir-from-lazy-arrays lazy-arrays))
         (program (buffer-program (first root-buffers))))
    (%make-cstate
     :backend backend
     :program program
     :allocations
     (petalisp.ir:compute-program-buffer-coloring program)
     :argument-buffers
     (mapcar
      (lambda (u) (car (rassoc u (petalisp.ir:program-leaf-alist program))))
      unknowns)
     :kernel-codelet-vector
     (let ((vector (make-array (petalisp.ir:program-number-of-kernels program))))
       (do-program-kernels (kernel program vector)
         (setf (svref vector (kernel-number kernel))
               (starpu-backend-kernel-codelet backend kernel)))))))

(defmethod starpu-backend-kernel-codelet ((starpu-backend starpu-backend) (kernel kernel))
  (let ((blueprint (kernel-blueprint
                    kernel
                    :scaling-threshold most-positive-fixnum
                    :offset-threshold most-positive-fixnum)))
    (alexandria:ensure-gethash
     blueprint
     (starpu-backend-blueprint-codelets starpu-backend)
     (blueprint-codelet blueprint))))
