;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

;;; An evaluator's dstate captures all state that is mutated during a
;;; particular invocation of that evaluator.

(defstruct (dstate
            (:constructor %make-dstate))
  ;; A simple vector with one StarPU data handle per buffer in the program.
  ;; Multiple entries of that vector can reference the StarPU same handle.
  (buffer-data-vector (alexandria:required-argument :buffer-data-vector)
   :type simple-vector
   :read-only t))

(defun make-dstate (cstate)
  )
