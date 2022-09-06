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
  (codelet-vector (alexandria:required-argument :codelet-vector)
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
     :codelet-vector (program-codelet-vector program (starpu-backend-codelet-cache backend)))))
