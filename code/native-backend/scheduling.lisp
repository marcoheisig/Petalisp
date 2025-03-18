(in-package #:petalisp.native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Invocations

(defstruct (invocation
            (:predicate invocationp)
            (:constructor make-invocation))
  "An invocation represents one run of a kernel on a particular iteration
space for some given source and target layouts."
  (kfn nil :type function :read-only t)
  (kernel nil :type kernel :read-only t)
  (iteration-space nil :type shape :read-only t)
  (targets nil :type (simple-array layout (*)))
  (sources nil :type (simple-array layout (*))))

(defun make-work-invocation (kernel-shard backend)
  (declare (kernel-shard kernel-shard) (backend backend))
  (make-invocation
   :kernel (kernel-shard-kernel kernel-shard)
   :iteration-space (kernel-shard-iteration-space kernel-shard)
   :sources (map 'vector #'buffer-shard-layout (kernel-shard-sources kernel-shard))
   :targets (map 'vector #'buffer-shard-layout (kernel-shard-targets kernel-shard))
   :kfn (compile-kernel backend (kernel-shard-kernel kernel-shard))))

(defun make-copy-invocation (iteration-space target source backend)
  (declare (shape iteration-space) (layout target source) (backend backend))
  (let* ((rank (shape-rank iteration-space))
         (ntype (layout-ntype source))
         (dummy-transformation (identity-transformation rank))
         (dummy-buffer (petalisp.ir:make-buffer
                        :depth 0
                        :shape iteration-space
                        :ntype ntype))
         (load (petalisp.ir::%make-load-instruction dummy-buffer dummy-transformation))
         (store (petalisp.ir::%make-store-instruction `((0 . ,load)) dummy-buffer dummy-transformation))
         (kernel
           (make-kernel
            :iteration-space iteration-space
            :instruction-vector (vector load store)
            :sources `((,dummy-buffer ,(stencil-from-instruction load)))
            :targets `((,dummy-buffer ,(stencil-from-instruction store))))))
    (setf (instruction-number load) 0)
    (setf (instruction-number store) 1)
    (make-invocation
     :kernel kernel
     :iteration-space iteration-space
     :targets (vector target)
     :sources (vector source)
     :kfn (compile-blueprint
           backend
           (kernel-blueprint kernel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Actions

(defstruct (action
            (:predicate actionp)
            (:constructor make-action))
  "An action consists of two lists of invocations that are processed by a worker:

1. A list of copy invocations that transfer data from the ghost layers of
   adjacent workers.

2. A list of work invocations that initialize the interior of their targets."
  (copy-invocations '() :type list)
  (work-invocations '() :type list))

;;; A hash table from layouts to actions defining the values in the interior of
;;; that layout.
(defvar *layout-action-table*)

;;; TODO
;;;
;;; Instead of creating exactly one action per layout, create one action per
;;; group of related buffer shards.  The main challenges in doing so is to find
;;; a good distribution of ghost layer copy invocations onto those actions, and
;;; to rewrite the scheduling code to map from layouts to lists of actions.

(defun merge-actions (action1 action2)
  (declare (action action1 action2))
  ;; Make the action with fewer work invocations obsolete and transfer those
  ;; work invocations to the other.
  (multiple-value-bind (old new)
      (if (<= (length (action-work-invocations action1))
              (length (action-work-invocations action2)))
          (values action1 action2)
          (values action2 action1))
    ;; Make each target layout within the old invocation point to the new
    ;; invocation.
    (loop for invocation in (action-work-invocations old) do
      (loop for target across (invocation-targets invocation) do
        (setf (gethash target *layout-action-table*) new)))
    ;; Merge all work invocations.
    (setf (action-work-invocations new)
          (append (action-work-invocations old)
                  (action-work-invocations new)))
    new))

(defun ensure-kernel-shard-invocation (kernel-shard backend)
  (declare (kernel-shard kernel-shard))
  #+(or) ;; quick daxpy/dgemm parallel hack
  (let* ((action (setf (gethash (list nil) *layout-action-table*)
                       (make-action)))
         (invocation (make-work-invocation kernel-shard backend)))
    (push invocation
          (action-work-invocations action))
    (return-from ensure-kernel-shard-invocation invocation))
  (let ((action nil))
    ;; Ensure that all targets of the worker have the same action.
    (dolist (target (kernel-shard-targets kernel-shard))
      (let ((target-action
              (alexandria:ensure-gethash
               (buffer-shard-layout target)
               *layout-action-table*
               (or action (make-action)))))
        (cond ((not action)
               (setf action target-action))
              ((not (eq action target-action))
               (merge-actions action target-action)))))
    ;; Add this kernel shard's work invocation if it isn't already present.
    (or (find-if
         (lambda (invocation)
           (declare (invocation invocation))
           (and (eq (invocation-kernel invocation)
                    (kernel-shard-kernel kernel-shard))
                (eq (invocation-iteration-space invocation)
                    (kernel-shard-iteration-space kernel-shard))))
         (action-work-invocations action))
        (let ((invocation (make-work-invocation kernel-shard backend)))
          (push invocation (action-work-invocations action))
          invocation))))

(defun ensure-actions (buffer-shard backend)
  (declare (buffer-shard buffer-shard))
  (loop for writer in (buffer-shard-writers buffer-shard) do
    (ensure-kernel-shard-invocation writer backend))
  (let ((split (buffer-shard-split buffer-shard)))
    (when split
      (ensure-actions (split-left-child split) backend)
      (ensure-actions (split-right-child split) backend))))

(defun compute-schedule (primogenitor-buffer-shard-vector backend)
  (let ((*layout-action-table* (make-hash-table :test #'eq))
        (graph (petalisp.scheduling:make-graph)))
    ;; Create all actions.
    (loop for primogenitor-buffer-shard across primogenitor-buffer-shard-vector do
      (ensure-actions primogenitor-buffer-shard backend))
    ;; Turn each action into a dependency graph node and create edges for each
    ;; other action it depends on.
    (maphash
     (lambda (layout action)
       (declare (ignore layout) (action action))
       (multiple-value-bind (node presentp)
           (petalisp.scheduling:graph-ensure-node graph action)
         (when (not presentp)
           (loop for invocation in (action-work-invocations action) do
             (loop for source across (invocation-sources invocation) do
               (multiple-value-bind (source-action presentp)
                   (gethash source *layout-action-table*)
                 ;; Skip the layout of leaf buffer shards.
                 (when (and presentp (not (eq source-action action)))
                   ;; Add one dependency to the action computing this source's
                   ;; interior.
                   (petalisp.scheduling:graph-add-edge
                    graph
                    (petalisp.scheduling:graph-ensure-node graph source-action)
                    node
                    (shape-size (invocation-iteration-space invocation)))
                   ;; Add dependencies to all actions that compute the values
                   ;; of this source's ghost layers.
                   (loop for (shape . layout) in (layout-ghost-layer-alist source) do
                     (multiple-value-bind (other-action presentp)
                         (gethash layout *layout-action-table*)
                       (when presentp
                         (petalisp.scheduling:graph-add-edge
                          graph
                          (petalisp.scheduling:graph-ensure-node graph other-action)
                          node
                          (shape-size shape))))))))))))
     *layout-action-table*)
    ;; Create the schedule.
    (let ((schedule
            (petalisp.scheduling:graph-parallel-depth-first-schedule
             graph
             (worker-pool-size (backend-worker-pool backend)))))
      ;; Have the first action that touches a particular piece of layout copy
      ;; over all its ghost layers.  This layout action table is destroyed in
      ;; this process.
      (loop for action-vector in schedule do
        (loop for action across action-vector do
          (when (actionp action)
            (loop for invocation in (action-work-invocations action) do
              (loop for layout across (invocation-sources invocation) do
                (when (nth-value 1 (gethash layout *layout-action-table*))
                  (remhash layout *layout-action-table*)
                  (loop for (iteration-space . neighbor-layout) in (layout-ghost-layer-alist layout) do
                    (push (make-copy-invocation iteration-space layout neighbor-layout backend)
                          (action-copy-invocations action)))))))))
      (when (member :dependencies *active-inspect-tags*)
        ;; view the dependency graph
        (let ((nodes (alexandria:hash-table-values (petalisp.scheduling::graph-object-nodes graph))))
          (if (< (length nodes) 1000)
              (funcall *inspect-dependencies* nodes)
              (error "Dependency graph too large to be visualized."))))
      schedule)))
