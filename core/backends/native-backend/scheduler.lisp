;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defclass default-scheduler-mixin ()
  ((%worker-queue :reader worker-queue
                  :initform (lparallel.queue:make-queue))
   (%worker-thread :initarg :worker-thread
                   :accessor worker-thread)))

(defmethod initialize-instance :after
    ((vm default-scheduler-mixin) &key &allow-other-keys)
  (flet ((work ()
           (loop (funcall (lparallel.queue:pop-queue (worker-queue vm))))))
    (setf (worker-thread vm)
          (bt:make-thread #'work :name "Petalisp Worker Thread"))))

;;; Instruct BACKEND to suitably set the STORAGE slot of IMMEDIATE.
(defgeneric vm/bind-memory (backend immediate))

;;; Instruct BACKEND to prepare the given BLUEPRINT for execution.
(defgeneric vm/compile (backend blueprint))

;;; Instruct BACKEND to compute the sequence of data structures
;;; GRAPH-ROOTS. Return the computed values of all GRAPH-ROOTS
(defgeneric vm/compute (backend graph-roots))

;;; Instruct BACKEND to execute the given KERNEL, assuming that all its
;;; sources and targets have already been allocated and computed.
(defgeneric vm/execute (backend kernel))

;;; Instruct BACKEND to reclaim the STORAGE of IMMEDIATE and set the
;;; STORAGE slot of IMMEDIATE to NIL.
(defgeneric vm/free-memory (backend immediate))

(defmethod compute-immediates
    ((data-structures list)
     (backend default-scheduler-mixin))
  (loop for immediate across (kernelize data-structures)
        collect (evaluate-naively immediate backend)))

(defun evaluate-naively (immediate backend)
  ;; only evaluate once
  (unless (storage immediate)
    ;; evaluate all dependencies
    (let (dependencies)
      (loop for kernel across (kernels immediate) do
        (loop for index from 1 below (length (kernel-references kernel))
              for source = (aref (kernel-references kernel) index) do
                (pushnew source dependencies)))
      (map nil (lambda (dependency) (evaluate-naively dependency backend))
           dependencies))
    ;; allocate memory
    (vm/bind-memory backend immediate)
    ;; compute all kernels
    (loop for kernel across (kernels immediate) do
      (vm/execute backend kernel)
      (loop for index from 1 below (length (kernel-references kernel))
            for source = (aref (kernel-references kernel) index) do
              (when (zerop (decf (refcount source)))
                (vm/free-memory backend source)))))
  immediate)
