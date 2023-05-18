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
  (let (;; In order to avoid processing the same buffer shard twice, we keep
        ;; track of scanned buffer shards in a hash table.  The values of that
        ;; table are lists of actions writing into that buffer shard.
        (table (make-hash-table :test #'eq))
        ;; The goal of this function is to turn all the supplied buffer shards
        ;; into a dependency graph of actions, which can then be passed to the
        ;; scheduler utility.
        (graph (petalisp.scheduling:make-graph))
        ;; In order to avoid consing, we collect the writers, targets, sources,
        ;; and weights in adjustable arrays with fill pointers.  These arrays
        ;; are filled and cleared once for each action that is being created.
        (writers (make-array 8 :adjustable t :fill-pointer 0))
        (targets (make-array 4 :adjustable t :fill-pointer 0))
        (sources (make-array 16 :adjustable t :fill-pointer 0))
        (weights (make-array 16 :adjustable t :fill-pointer 0))
        (vicinities (make-array 16 :adjustable t :fill-pointer 0)))
    (labels
        ((process-buffer-shard (buffer-shard)
           (declare (buffer-shard buffer-shard))
           (cond ((and (null (buffer-shard-writers buffer-shard))
                       (null (buffer-shard-readers buffer-shard)))
                  (let ((split (buffer-shard-split buffer-shard)))
                    (assert split)
                    (process-buffer-shard (split-left-child split))
                    (process-buffer-shard (split-right-child split))))
                 ((null (buffer-shard-writers buffer-shard))
                  (assert (leaf-buffer-p (buffer-shard-buffer buffer-shard))))
                 (t (ensure-actions buffer-shard))))
         (ensure-actions (buffer-shard)
           (declare (buffer-shard buffer-shard))
           (multiple-value-bind (actions presentp)
               (gethash buffer-shard table)
             (when presentp (return-from ensure-actions actions)))
           ;; Determine all targets, writers, and sources relating directly
           ;; to this node.  Turn them into an action.
           (scan-target buffer-shard)
           ;; Compute the neighbor relationships of all sources.
           (adjust-array vicinities (array-total-size sources) :fill-pointer (fill-pointer sources))
           (map-into vicinities #'compute-buffer-shard-vicinity sources)
           (let ((action
                   (make-action
                    :copy-invocations (compute-copy-invocations sources vicinities backend)
                    :work-invocations (compute-work-invocations writers backend))))
             ;; Set the actions of each target.
             (loop for target across targets do
               (setf (gethash target table)
                     (list* action (child-actions target))))
             ;; Turn the action into a graph node, and create edges to each
             ;; action of each source.
             (let ((node (petalisp.scheduling:graph-ensure-node graph action)))
               ;; Each edge has a weight that is the number of kernel
               ;; iterations loading from it.
               (adjust-array weights (array-total-size sources) :fill-pointer (fill-pointer sources))
               (fill weights 0)
               (loop for writer in (buffer-shard-writers buffer-shard) do
                 (let ((niterations (shape-size (kernel-shard-iteration-space writer))))
                   (loop for source in (kernel-shard-sources writer) do
                     (incf (aref weights (position source sources))
                           niterations))))
               ;; Add one edge for each action of each source and its vicinity.
               (loop
                 ;; TODO Improve weights.
                 for source across sources
                 for weight across weights
                 for vicinity across vicinities do
                   (loop for other-action in (gethash source table) do
                     (let ((other-node (petalisp.scheduling:graph-ensure-node graph other-action)))
                       (petalisp.scheduling:graph-add-edge graph other-node node weight)))
                   (loop for axis below (shape-rank (buffer-shard-shape source)) do
                     (loop for neighbor in (vicinity-left-neighbors vicinity axis) do
                       (loop for other-action in (gethash neighbor table) do
                         (let ((other-node (petalisp.scheduling:graph-ensure-node graph other-action)))
                           (petalisp.scheduling:graph-add-edge graph other-node node 1))))
                     (loop for neighbor in (vicinity-right-neighbors vicinity axis) do
                       (loop for other-action in (gethash neighbor table) do
                         (let ((other-node (petalisp.scheduling:graph-ensure-node graph other-action)))
                           (petalisp.scheduling:graph-add-edge graph other-node node 1))))))
               ;; Cleanup.
               (setf (fill-pointer writers) 0)
               (setf (fill-pointer targets) 0)
               (setf (fill-pointer sources) 0)
               (setf (fill-pointer weights) 0)
               (setf (fill-pointer vicinities) 0)
               (gethash buffer-shard table))))
         (child-actions (buffer-shard)
           (declare (buffer-shard buffer-shard))
           (let ((split (buffer-shard-split buffer-shard)))
             (if (not split)
                 '()
                 (append
                  (ensure-actions (split-left-child split))
                  (ensure-actions (split-right-child split))))))
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
      (map nil #'process-buffer-shard primogenitor-buffer-shard-vector))
    #+(or)
    (petalisp.graphviz:view
     (alexandria:hash-table-values
      (petalisp.scheduling::graph-object-nodes graph)))
    (petalisp.scheduling:graph-parallel-depth-first-schedule
     graph
     (worker-pool-size (backend-worker-pool backend)))))

(defun compute-copy-invocations (buffer-shards vicinities backend)
  (declare (vector buffer-shards))
  (let ((copy-invocations '()))
    (flet ((collect-copy-invocation (source target)
             (declare (buffer-shard source target))
             (let* ((source-buffer (buffer-shard-buffer source))
                    (target-buffer (buffer-shard-buffer target))
                    (iteration-space
                      (shape-intersection
                       (buffer-shard-domain source)
                       (buffer-shard-shape target)))
                    (rank (shape-rank iteration-space))
                    (lt (identity-transformation rank))
                    (st (identity-transformation rank))
                    (load (petalisp.ir::%make-load-instruction source-buffer lt))
                    (store (petalisp.ir::%make-store-instruction (cons 0 load) target-buffer st)))
               (unless (empty-shape-p iteration-space)
                 (push
                  (make-invocation
                   :kernel (make-kernel
                            :iteration-space iteration-space
                            :sources `((,source-buffer ,(make-stencil (list load))))
                            :targets `((,target-buffer ,store)))
                   :iteration-space iteration-space
                   :sources (vector (buffer-shard-layout source))
                   :targets (vector (buffer-shard-layout target))
                   :kfn (backend-compile-blueprint
                         backend
                         (make-copy-blueprint iteration-space source target)))
                  copy-invocations)))))
      (loop for buffer-shard across buffer-shards
            for vicinity across vicinities
            do (let ((rank (shape-rank (buffer-shard-shape buffer-shard))))
                 (loop for axis below rank do
                   (loop for neighbor in (vicinity-left-neighbors vicinity axis) do
                     (collect-copy-invocation neighbor buffer-shard))
                   (loop for neighbor in (vicinity-right-neighbors vicinity axis) do
                     (collect-copy-invocation neighbor buffer-shard))))))
    copy-invocations))

(defun make-copy-blueprint (iteration-space source target)
  (declare (shape iteration-space) (buffer-shard source target))
  (let* ((source-layout (buffer-shard-layout source))
         (target-layout (buffer-shard-layout target))
         (rank (layout-rank source-layout))
         (ntype (storage-ntype (layout-storage source-layout)))
         (offsets (make-list rank :initial-element 0)))
    (assert (= (layout-rank target-layout) rank))
    (assert (typo:ntype= (storage-ntype (layout-storage target-layout)) ntype))
    (ucons:ulist
     ;; Iteration space.
     (iteration-space-blueprint iteration-space)
     ;; Targets.
     (ucons:ulist
      (ucons:ulist ntype (transformation-blueprint (identity-transformation rank))))
     ;; Sources.
     (ucons:ulist
      (ucons:ulist ntype (transformation-blueprint (identity-transformation rank))))
     ;; Load.
     (apply #'ucons:ulist :load 0 0 offsets)
     ;; Store.
     (ucons:ulist :store (ucons:ulist 0 0) 0 0))))

(defun compute-work-invocations (kernel-shards backend)
  (declare (vector kernel-shards))
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
