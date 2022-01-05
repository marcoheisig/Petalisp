;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; In this file, we partition each kernel into tasks.  We do this by first
;;; computing the optimal dimensions of a task, and then cut the domain
;;; into tasks that are at most as large as that.

(defgeneric optimal-task-dimensions (kernel processor backend))

;;; The default method for computing the optimal dimensions of a task is to
;;; chose a hypercube such that all loads and stores fit into that task's
;;; share of the caches.
;;;
;;; TODO: More sophisticated partitioning schemes are conceivable.  This is
;;; just a simple version to get things going.
(defmethod optimal-task-dimensions (kernel processor backend)
  (let* ((rank (shape-rank (kernel-iteration-space kernel)))
         (size (if (zerop rank)
                   (return-from optimal-task-dimensions '())
                   (shape-size (kernel-iteration-space kernel))))
         (machine (backend-machine backend))
         (main-memory (machine-main-memory machine))
         (cache-size
           (loop for memory = (processor-memory processor)
                   then (memory-parent memory)
                 until (or (null memory)
                           (eq memory main-memory))
                 sum (floor (memory-size memory)
                            (length
                             (memory-processors memory)))))
         (bytes-per-element
           (let ((bits 0.0))
             (flet ((count-bits (buffer)
                      (incf bits
                            ;; In case the size of the buffer is smaller
                            ;; than the size of the kernel iteration space,
                            ;; we scale down the number of transferred
                            ;; bytes accordingly.
                            (* (float (buffer-size buffer))
                               (/ (float size))
                               (float
                                (petalisp.type-inference:ntype-size
                                 (buffer-ntype buffer)))))))
               (map-kernel-inputs #'count-bits kernel)
               (map-kernel-outputs #'count-bits kernel))
             ;; Ensure that the bytes per elements can never be zero.
             (min 1.0 (/ bits 8)))))
    (make-list
     rank
     :initial-element
     (floor
      (expt (/ cache-size bytes-per-element)
            (/ rank))))))
