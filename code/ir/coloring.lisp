(in-package #:petalisp.ir)

(defun compute-program-buffer-coloring (program)
  "Returns a partitioning of all non-leaf buffers in PROGRAM into a list of
lists of buffers.  Each list of buffers has the same color, meaning that
they can share the same memory location without conflicts.

All root buffers conflict with each other, so each list of buffers contains
at most one root buffer.  By convention, if a root buffer is present in a
list of buffers, it comes first."
  (declare (program program))
  (let* ((result '())
         ;; A vector with one bit per buffer.  An entry of 0 means we
         ;; currently don't care about this buffer in terms of liveness and
         ;; allocation.  An entry of 1 means we do.
         (buffer-activep-vector
           (make-array (program-number-of-buffers program)
                       :element-type 'bit
                       :initial-element 0))
         ;; A vector with one list of buffers per task.  It is used to
         ;; store the liveness of buffers at each task.
         (task-live-buffers-vector
           (make-array (program-number-of-tasks program)
                       :initial-element '())))
    (petalisp.ir:do-program-buffer-groups (buffers program)
      (cond
        ;; If there is just a single buffer of that shape and ntype, there
        ;; is no need to perform graph coloring.  We simply mark it for
        ;; allocation.
        ((= 1 (length buffers))
         (push buffers result))
        ;; If there is more than one buffer, we have to determine a valid
        ;; coloring.
        (t
         ;; Mark the buffers of the current buffer group as being active.
         (dolist (buffer buffers)
           (setf (aref buffer-activep-vector (buffer-number buffer)) 1))
         ;; Determine the active buffers that are live at each task.  We
         ;; exploit the fact that the tasks in the task vector are sorted
         ;; in depth-first dependency order, so a single scan from right to
         ;; left solves the data flow problem for each task correctly.
         (loop for index from (1- (program-number-of-tasks program)) downto 0
               for task = (svref (program-task-vector program) index)
               do (let ((live-buffers '()))
                    (flet ((collect (buffer)
                             (when (plusp (aref buffer-activep-vector (buffer-number buffer)))
                               (do-task-defined-buffers (defined-buffer task)
                                 (when (eq defined-buffer buffer)
                                   (return-from collect)))
                               (pushnew buffer live-buffers))))
                      ;; The live buffers at a task are those buffers that
                      ;; are live at any of its successors, plus those used
                      ;; by it, minus those defined by it.
                      (unless (null (task-successors task))
                        (mapc #'collect (svref task-live-buffers-vector (1+ index))))
                      (do-task-kernels (kernel task)
                        (map-kernel-inputs #'collect kernel)))
                    (setf (aref task-live-buffers-vector (task-number task))
                          live-buffers)))
         ;; Build the conflict graph of buffers that must not be allocated
         ;; in the same location.
         (let ((cgraph (petalisp.utilities:make-cgraph)))
           ;; Add the conflicts for each task.  Each active buffer defined
           ;; by a task conflicts with each other active buffer defined by
           ;; that task, and with each other active buffer that is live
           ;; while the task is being executed.
           (do-program-tasks (task program)
             (do-task-defined-buffers (defined-buffer task)
               (unless (zerop (aref buffer-activep-vector (buffer-number defined-buffer)))
                 (let ((defined-buffer-cnode (petalisp.utilities:cgraph-ensure-cnode cgraph defined-buffer)))
                   (loop for live-buffer in (svref task-live-buffers-vector (task-number task)) do
                     (petalisp.utilities:cgraph-add-conflict
                      cgraph
                      defined-buffer-cnode
                      (petalisp.utilities:cgraph-ensure-cnode cgraph live-buffer)))
                   (do-task-defined-buffers (other-buffer task)
                     (unless (zerop (aref buffer-activep-vector (buffer-number other-buffer)))
                       (when (< (buffer-number other-buffer) (buffer-number defined-buffer))
                         (petalisp.utilities:cgraph-add-conflict
                          cgraph
                          defined-buffer-cnode
                          (petalisp.utilities:cgraph-ensure-cnode cgraph other-buffer)))))))))
           ;; Add conflicts between all root buffers.
           (loop for (buffer1 . more-buffers) on buffers when (root-buffer-p buffer1) do
             (let ((buffer1-cnode (petalisp.utilities:cgraph-ensure-cnode cgraph buffer1)))
               (loop for buffer2 in more-buffers when (root-buffer-p buffer2) do
                 (let ((buffer2-cnode (petalisp.utilities:cgraph-ensure-cnode cgraph buffer2)))
                   (petalisp.utilities:cgraph-add-conflict cgraph buffer1-cnode buffer2-cnode)))))
           ;; Color the graph.  All buffers of the same color can be placed
           ;; in the same allocation.
           (loop for buffers across (petalisp.utilities:cgraph-coloring cgraph) do
             ;; Ensure that if there is a root buffer among that list of
             ;; buffers, it occurs first.
             (loop for cons on buffers do
               (when (root-buffer-p (car cons))
                 (rotatef (car cons) (car buffers))))
             (push buffers result)))
         ;; Finally, mark the buffers of the current buffer group as being
         ;; inactive again.
         (dolist (buffer buffers)
           (setf (aref buffer-activep-vector (buffer-number buffer)) 0)))))
    result))
