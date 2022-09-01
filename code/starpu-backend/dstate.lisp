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
  (let ((program (cstate-program cstate))
        (allocations (cstate-allocations cstate)))
    (%make-dstate
     :buffer-data-vector
     (let ((vector (make-array (program-number-of-buffers program))))
       (dolist (buffers allocations vector)
         (let ((handle (make-starpu-handle (first buffers))))
           (dolist (buffer buffers)
             (setf (svref vector (buffer-number buffer))
                   handle))))))))

(defun make-starpu-handle (buffer)
  (let ((element-type (petalisp.type-inference:type-specifier (buffer-ntype buffer))))
    (trivia:ematch (shape-ranges (buffer-shape buffer))
      ((list)
       (starpu:make-vector :nx 1 :element-type element-type))
      ((list (range a))
       (starpu:make-vector :nx a :element-type element-type))
      ((list (range a) (range b))
       (starpu:make-matrix :ny a :nx b :element-type element-type))
      ((list (range a) (range b) (range c))
       (starpu:make-block :nz a :ny b :nx c :element-type element-type))
      ((list* (range a) (range b) (range c) _)
       (starpu:make-block :nz a :ny b :nx c :element-type element-type)))))
