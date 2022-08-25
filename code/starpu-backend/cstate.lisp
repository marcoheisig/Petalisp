;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

;;; An evaluator's cstate captures all state that is constant with respect
;;; to a particular invocation of that evaluator.  For example, the number,
;;; shapes, and element types of the evaluator's arguments is part of the
;;; cstate.

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
   :read-only t))

(defun make-cstate (backend unknowns lazy-arrays)
  (let* ((root-buffers (ir-from-lazy-arrays lazy-arrays))
         (program (buffer-program (first root-buffers)))
         (allocations (petalisp.ir:compute-program-buffer-coloring program)))
    (%make-cstate
     :backend backend
     :program program
     :unknowns unknowns
     :allocations allocations)))


