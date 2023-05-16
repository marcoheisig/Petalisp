;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defstruct (invocation
            (:predicate invocationp)
            (:constructor make-invocation))
  (kfn nil :type function :read-only t)
  (kernel nil :type kernel :read-only t)
  (iteration-space nil :type shape :read-only t)
  (targets nil :type (simple-array layout (*)))
  (sources nil :type (simple-array layout (*))))

(defstruct (action
            (:predicate actionp)
            (:constructor make-action))
  (copy-invocations (alexandria:required-argument :copies)
   :type list
   :read-only t)
  (work-invocations (alexandria:required-argument :calls)
   :type list
   :read-only t))

(defun compute-schedule (primogenitor-buffer-shard-vector backend)
  (let ((table (make-hash-table :test #'eq))
        (graph (petalisp.scheduling:make-graph))
        (writers (make-array 8 :adjustable t :fill-pointer 0))
        (targets (make-array 4 :adjustable t :fill-pointer 0))
        (sources (make-array 16 :adjustable t :fill-pointer 0))
        (weights (make-array 16 :adjustable t :fill-pointer 0)))
    (labels
        ((ensure-buffer-shard-action (buffer-shard)
           (declare (buffer-shard buffer-shard))
           (or (gethash buffer-shard table)
               (with-slots (readers writers) buffer-shard
                 ;; Determine all targets, writers, and sources of this action.
                 (scan-target buffer-shard)
                 ;; Create the action.
                 (let ((action
                         (make-action
                          :copy-invocations (compute-copy-invocations sources backend)
                          :work-invocations (compute-work-invocations writers backend))))
                   ;; Register the action for all targets.
                   (loop for target across targets do
                     (setf (gethash target table) action))
                   ;; Turn the action into a graph node, and create edges to
                   ;; the action of each source.
                   (let ((node (petalisp.scheduling:graph-ensure-node graph action)))
                     ;; Each edge has a weight that is the number of kernel
                     ;; iterations loading from it.
                     (adjust-array weights (array-total-size sources)
                                   :fill-pointer (fill-pointer sources))
                     (fill weights 0)
                     (loop for writer in writers do
                       (let ((niterations (shape-size (kernel-shard-iteration-space writer))))
                         (loop for source in (kernel-shard-sources writer) do
                           (incf (aref weights (position source sources))
                                 niterations))))
                     ;; Add one edge for each source.
                     (loop for source across sources for weight across weights do
                       (let* ((other-action (gethash source table))
                              (other-node
                                (petalisp.scheduling:graph-ensure-node graph other-action)))
                         (petalisp.scheduling:graph-add-edge graph node other-node weight)))
                     ;; Done.
                     action)))))
         (scan-target (target)
           (unless (find target targets)
             (vector-push-extend target targets)
             (map nil #'scan-writer (buffer-shard-writers target))))
         (scan-source (source)
           (unless (find source sources)
             (vector-push-extend source sources)))
         (scan-writer (writer)
           (unless (find writer writers)
             (vector-push-extend writer writers)
             (map nil #'scan-target (kernel-shard-targets writer))
             (map nil #'scan-source (kernel-shard-sources writer)))))
      ;; We exploit that buffer numbers are handed out so that a traversal of a
      ;; program's buffers in ascending order of numbers respects all data
      ;; dependencies.
      (map nil #'ensure-buffer-shard-action primogenitor-buffer-shard-vector))
    (petalisp.scheduling:graph-parallel-depth-first-schedule
     graph
     (worker-pool-size (backend-worker-pool backend)))))

(defun compute-copy-invocations (buffer-shards backend)
  (declare (vector buffer-shards))
  (let ((copy-invocations '()))
    (flet ((collect-copy-invocation (from to)
             (declare (buffer-shard from to))
             (let ((iteration-space
                     (shape-intersection
                      (buffer-shard-shape from)
                      (buffer-shard-shape to))))
               (push
                (make-invocation
                 :kernel (make-kernel :iteration-space iteration-space)
                 :iteration-space iteration-space
                 :sources (vector from)
                 :targets (vector to)
                 :kfn (backend-compile-blueprint
                       backend
                       (make-copy-blueprint iteration-space from to)))
                copy-invocations))))
      (loop for buffer-shard across buffer-shards do
        (let* ((vicinity (compute-buffer-shard-vicinity buffer-shard))
               (rank (shape-rank (buffer-shard-shape buffer-shard))))
          (loop for axis below rank do
            (loop for neighbor in (vicinity-left-neighbors vicinity axis) do
              (collect-copy-invocation neighbor buffer-shard))
            (loop for neighbor in (vicinity-right-neighbors vicinity axis) do
              (collect-copy-invocation neighbor buffer-shard))))))
    copy-invocations))

(defun make-copy-blueprint (iteration-space from to)
  (let* ((rank (layout-rank from))
         (ntype (storage-ntype (layout-storage from)))
         (offsets (make-list rank :initial-element 0)))
    (assert (= (layout-rank to) rank))
    (assert (typo:ntype= (storage-ntype (layout-storage to)) ntype))
    (ucons:ulist
     ;; Iteration space.
     (iteration-space-blueprint iteration-space)
     ;; Targets.
     (ucons:ulist
      (ucons:ulist ntype (transformation-blueprint (identity-transformation rank))))
     ;; Sources.
     (ucons:ulist
      (ucons:ulist ntype (transformation-blueprint (identity-transformation rank))))
     (apply #'ucons:ulist :load 0 0 offsets)
     (ucons:ulist :store 0 0 0))))

(defun compute-work-invocations (kernel-shards backend)
  (declare (simple-vector kernel-shards))
  (loop for kernel-shard across kernel-shards
        collect
        (make-invocation
         :kernel (kernel-shard-kernel kernel-shard)
         :iteration-space (kernel-shard-iteration-space kernel-shard)
         :sources (map 'vector #'buffer-shard-layout (kernel-shard-sources kernel-shard))
         :targets (map 'vector #'buffer-shard-layout (kernel-shard-targets kernel-shard))
         :kfn (backend-compile-blueprint
               backend
               (kernel-blueprint (kernel-shard-kernel kernel-shard))))))
