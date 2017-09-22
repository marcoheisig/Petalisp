;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *global-scheduler-queue* (make-queue)
  "A FIFO queue of lists of data structures.")

(defvar *global-scheduler-thread*
  (make-thread #'global-scheduler
               :name "*global-scheduler-thread*"))

(defmethod schedule (&rest objects)
  (if-let ((unsuitable-arguments (remove-if #'data-structure? objects)))
    (simple-program-error
     "Unsuitable argument~s to enqueue:~%~{  ~S~}"
     (length unsuitable-arguments) unsuitable-arguments)
    (queue-enqueue *global-scheduler-queue* objects)))

(defmethod compute (&rest objects)
  (apply #'enqueue objects)
  ;; wait until all computations are immediates
  )

(defun global-scheduler ()
  "Entry function for the global scheduler thread. Arranges for each list
of data structures in the *GLOBAL-SCHEDULER-QUEUE* to be computed in the
order of enqueuing. It should never terminate."
  (loop
    (let ((kernels (kernelize (queue-dequeue *global-scheduler-queue*))))
      (iterate (for ready-kernels = (remove-if #'dependencies kernels))
               (while ready-kernels)
               (setf kernels (set-difference kernels ready-kernels :test #'eq))
               (mapc #'evaluate-kernel ready-kernels)))))

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
evaluation of the given GRAPH-ROOTS."
  (let ((use-table (make-hash-table :test #'eq)))
    (flet ((users (node) (gethash node use-table)))
      (labels ((populate-use-table (node)
                 (if-let ((users (users node)))
                   (unless (member node (inputs (first users)))
                     (dolist (user users)
                       (push node (gethash input use-table)))))
                 (let ((used-nodes (inputs node)))
                   (if (member node (gethash (first used-nodes) use-table))))
                 (dolist (input (inputs node))
                   (let ((users (gethash input use-table)))
                     (unless (member node users)
                       (if users
                           (push node (gethash input use-table))
                           (setf (gethash input use-table) (list node)))
                       (mapc #'populate-use-table (inputs node)))))))
        (mapcar #'populate-use-table objects)))
    (labels ((dependencies (node)
               (let (dependencies)
                 (labels ((recurse (node)
                            (if (leaf? node)
                                (push node dependencies)
                                (mapc #'dependencies (inputs node)))))
                   (recurse node))))
             (recurse (node)
               (unless (leaf? node)
                 (if (or (< 1 (length (gethash node use-table)))
                         (member node objects :test #'eq))
                     (let ((recipe (shallow-copy node)))
                       (mapc #'recurse (inputs recipe))
                       (change-class node 'strided-array-elaboration
                                     :dependencies (dependencies recipe)
                                     :recipe recipe))
                     (mapc #'recurse (inputs node))))))
      (mapc #'recurse objects))))
