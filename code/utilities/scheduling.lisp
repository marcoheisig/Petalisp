;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(defpackage #:petalisp.scheduling
  (:use #:common-lisp)
  (:export
   #:node
   #:nodep
   #:node-predecessors
   #:node-successors
   #:node-depth
   #:node-height
   #:make-graph
   #:graph
   #:graphp
   #:graph-initial-nodes
   #:graph-final-nodes
   #:graph-object-nodes
   #:graph-ensure-node
   #:graph-add-edge
   #:graph-parallel-depth-first-schedule))

(in-package #:petalisp.scheduling)

(defstruct (node
            (:predicate nodep)
            (:constructor make-node))
  ;; A (node . weight) alist of incoming edges.
  (predecessor-alist '() :type list)
  ;; A (node . weight) alist of outgoing edges.
  (successor-alist '() :type list)
  ;; The object that is managed by this node.
  (object nil :type t)
  ;; The distance of the longest path to a node with zero predecessors.
  (depth 0 :type unsigned-byte)
  ;; The distance of the longest path to a node with zero successors.
  (height 0 :type unsigned-byte)
  ;; A measure that is dynamically adapted during scheduling to prioritize
  ;; nodes that lead to good data locality.
  (eagerness 0 :type unsigned-byte)
  ;; The number of the worker that this node has been scheduled on, or, if the
  ;; node hasn't been scheduled yet, the node it should ideally be scheduled
  ;; to, or NIL if there is no clear affinity to any worker.
  (affinity nil :type (or unsigned-byte null))
  ;; A counter used for various purposes during scheduling.
  (counter 0 :type unsigned-byte)
  ;; During scheduling, this slot tracks the node's queue position so that it
  ;; its priority can be adapted more cheaply.
  (position nil))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~S" (node-object node))))

(defun copy-node-into (node other-node)
  (declare (node node other-node))
  (setf (node-predecessor-alist other-node)
        (node-predecessor-alist node))
  (setf (node-successor-alist other-node)
        (node-successor-alist node))
  (setf (node-object other-node)
        (node-object node))
  (setf (node-depth other-node)
        (node-depth node))
  (setf (node-height other-node)
        (node-height node))
  (setf (node-eagerness other-node)
        (node-eagerness node))
  (setf (node-counter other-node)
        (node-counter node))
  (setf (node-position other-node)
        (node-position node)))

(defun node-more-important-p (node1 node2)
  (declare (node node1 node2))
  (block nil
    (flet ((cmp (p1 p2)
             (cond ((> p1 p2) (return t))
                   ((< p1 p2) (return nil)))))
      ;; The primary criterion is the node's depth, since we want to create a
      ;; depth-first schedule.
      (cmp (node-depth node1)
           (node-depth node2))
      ;; The secondary criterion is the node's height.  Doing so has the
      ;; advantage that, when using the depth-first schedule as a basis for
      ;; generating a parallel schedule, that parallel schedule will likely
      ;; have a lower time to solution.
      (cmp (node-height node1)
           (node-height node2))
      ;; The tertiary criterion is to chose nodes that maximize data locality.  The
      ;; eagerness is a property that is constantly updated whenever
      ;; neighboring nodes are scheduled.
      (cmp (node-eagerness node1)
           (node-eagerness node2))
      ;; Otherwise, the nodes are equally important.
      nil)))

(defstruct (graph
            (:predicate graphp)
            (:constructor make-graph))
  ;; A hash-table, mapping objects to their nodes.
  (object-nodes (make-hash-table) :type hash-table))

(defun graph-ensure-node (graph object)
  (declare (graph graph))
  (alexandria:ensure-gethash
   object
   (graph-object-nodes graph)
   (make-node :object object)))

(defun graph-add-edge (graph from to weight)
  (declare (graph graph) (node from to))
  (declare (ignore graph))
  (assert (not (eq from to)))
  (push (cons from weight) (node-predecessor-alist to))
  (push (cons to weight) (node-successor-alist from)))

(defun graph-compute-node-height (graph)
  (graph-compute-distances
   graph
   #'node-height
   #'(setf node-height)
   #'node-successor-alist
   #'node-predecessor-alist))

(defun graph-compute-node-depth (graph)
  (graph-compute-distances
   graph
   #'node-depth
   #'(setf node-depth)
   #'node-predecessor-alist
   #'node-successor-alist))

(defun graph-compute-distances (graph get-distance set-distance input-alist output-alist)
  (flet ((node-distance (node)
           (funcall get-distance node))
         ((setf node-distance) (value node)
           (funcall set-distance value node)))
    (let ((initial-nodes '()))
      ;; Initialize the nodes and the worklist.
      (loop for node being the hash-values of (graph-object-nodes graph) do
        (let ((n (length (funcall input-alist node))))
          (setf (node-counter node) n)
          (when (zerop n)
            (push node initial-nodes))))
      ;; Compute the distance to each node.
      (let ((worklist initial-nodes))
        (loop until (null worklist) for node = (pop worklist) do
          (setf (node-distance node)
                (1+ (reduce #'max (funcall input-alist node)
                              :key (alexandria:compose #'node-distance #'car)
                              :initial-value 0)))
          (loop for (output . weight) in (funcall output-alist node) do
            ;; Decrement counter.
            (when (zerop (decf (node-counter output)))
              (push output worklist)))))
      initial-nodes)))

(defstruct (pdfs-queue
            (:predicate pdfs-queue-p)
            (:constructor make-pdfs-queue ()))
  ;; The priority queue determines the order in which nodes are scheduled.
  (priority-queue (queues:make-queue :priority-queue :compare #'node-more-important-p)
   :type queues:priority-queue
   :read-only t)
  ;; When updating node eagerness in the priority queue, we use this temporary
  ;; node as non-consing intermediate storage.
  (tmpnode (make-node) :type node))

(defun pdfs-queue-push (pdfs-queue node)
  (declare (pdfs-queue pdfs-queue) (node node))
  (assert (not (node-position node)))
  ;; Determine whether the node has an affinity to a particular worker.
  (with-slots (affinity predecessor-alist) node
    (when (not affinity)
      (let ((total 0)
            (max-weight 0)
            (max-affinity nil))
        (loop for (predecessor . weight) in predecessor-alist do
          (incf total weight)
          (when (> weight max-weight)
            (setf max-weight weight)
            (setf max-affinity (node-affinity predecessor))))
        (when (> (* 2 max-weight) total)
          (setf affinity max-affinity)))))
  ;; Push the node to the priority queue and record its position therein.
  (with-slots (priority-queue) pdfs-queue
    (setf (node-position node)
          (nth-value 1 (queues:qpush priority-queue node)))
    node))

(defun pdfs-queue-pop (pdfs-queue)
  (declare (pdfs-queue pdfs-queue))
  (with-slots (priority-queue) pdfs-queue
    (let ((node (queues:qpop priority-queue)))
      (setf (node-position node) nil)
      node)))

(defun pdfs-queue-size (pdfs-queue)
  (declare (pdfs-queue pdfs-queue))
  (queues:qsize (pdfs-queue-priority-queue pdfs-queue)))

(defun pdfs-queue-increase-node-eagerness (pdfs-queue node increment)
  (with-slots (priority-queue tmpnode) pdfs-queue
    ;; This function is complicated by the fact that we want to avoid
    ;; consing when updating the queue position.
    (if (not (node-position node))
        (incf (node-eagerness node) increment)
        (let ((position (node-position node)))
          (copy-node-into node tmpnode)
          (incf (node-eagerness tmpnode) increment)
          (queues:queue-change priority-queue position tmpnode)
          (incf (node-eagerness node) increment)
          (setf (queues::node-value position) node)))))

(defun graph-parallel-depth-first-schedule (graph p)
  "Returns a list of vectors, where each vector describes one step of the
schedule.  Each vector has length P, and elements that are either NIL or one of
the objects that have been scheduled.  The schedule ensures that each node
appears later than all its predecessors, and that all entries of one vector can
be executed in parallel.

This function uses the P-DFS algorithm from Blelloch, Gibbons, and
Matias (https://doi.org/10.1145/301970.301974), with some augmentations to
improve data locality."
  (declare (graph graph) (type (integer 1) p))
  (graph-compute-node-depth graph)
  (graph-compute-node-height graph)
  ;; Maintain a priority queue of all nodes that are ready to be scheduled.
  (let ((pdfs-queue (make-pdfs-queue)))
    ;; Initialize node counters and the queue.
    (loop for node being the hash-values of (graph-object-nodes graph) do
      (let ((n (length (node-predecessor-alist node))))
        (setf (node-counter node) n)
        (setf (node-affinity node) nil)
        (setf (node-eagerness node) 0)
        (when (zerop n)
          (pdfs-queue-push pdfs-queue node))))
    ;; Generate the schedule.
    (petalisp.utilities:with-collectors ((steps collect-step))
      ;; When collecting a node vector, replace all nodes therein with the
      ;; corresponding objects.
      (flet ((collect-node-vector (node-vector)
               (loop for index below p do
                 (symbol-macrolet ((place (aref node-vector index)))
                   (when (nodep place)
                     (setf place (node-object place)))))
               (collect-step node-vector)))
        (let ((current-node-vector (make-array p :initial-element nil))
              (ensuing-node-vector (make-array p :initial-element nil))
              (current-nodes 0)
              (ensuing-nodes 0)
              ;; Nodes that will be scheduled in this step.
              (node-stack (make-array p :initial-element nil :fill-pointer 0))
              ;; The workers that will pick work from the node stack.
              (worker-stack (make-array p :initial-element nil :fill-pointer 0)))
          (declare (unsigned-byte current-nodes ensuing-nodes))
          (loop until (zerop (pdfs-queue-size pdfs-queue)) do
            ;; Fill one node vector.
            (loop until (= (+ current-nodes (length node-stack)) p) do
              (when (zerop (pdfs-queue-size pdfs-queue))
                ;; If control reaches this point, there aren't enough nodes
                ;; ready in the queue to fill the current node vector.
                ;; However, we can still try to steal some nodes from the
                ;; ensuing node vector.  This will ruin their affinity, but
                ;; scheduling them earlier will make up for that.
                (loop for index below p until (zerop ensuing-nodes) do
                  (when (nodep (aref ensuing-node-vector index))
                    (vector-push (shiftf (aref ensuing-node-vector index) nil) node-stack)
                    (decf ensuing-nodes)
                    (incf current-nodes)))
                (loop-finish))
              ;; Pick one node from the queue and place it either in a location
              ;; it has affinity to, or in the node stack.
              (let* ((node (pdfs-queue-pop pdfs-queue))
                     (affinity (node-affinity node)))
                (cond ((null affinity)
                       (vector-push node node-stack))
                      ((null (aref current-node-vector affinity))
                       (setf (aref current-node-vector affinity) node)
                       (incf current-nodes))
                      ((null (aref ensuing-node-vector affinity))
                       (setf (aref ensuing-node-vector affinity) node)
                       (incf ensuing-nodes))
                      (t
                       (vector-push node node-stack)))))
            ;; Determine the workers that have anything to do, yet.
            (loop for index below p do
              (when (not (aref current-node-vector index))
                (vector-push index worker-stack)))
            ;; Shuffle the worker stack.
            (let ((end (length worker-stack)))
              (loop for index below end do
                (rotatef (aref worker-stack index)
                         (aref worker-stack (random end)))))
            ;; Use nodes from the node stack to fill workers that don't have
            ;; anything to do, yet.
            (loop for worker across worker-stack
                  for node across node-stack
                  do (setf (node-affinity node) worker)
                     (setf (aref current-node-vector worker) node)
                     (incf current-nodes))
            (setf (fill-pointer worker-stack) 0)
            (setf (fill-pointer node-stack) 0)
            ;; Mark all nodes in the current node vector as completed.
            (loop for elt across current-node-vector do
              (when (nodep elt)
                (loop for (successor . weight) in (node-successor-alist elt) do
                  (when (zerop (decf (node-counter successor)))
                    (pdfs-queue-push pdfs-queue successor)))))
            ;; Prepare the stage for the next iteration
            (shiftf current-nodes ensuing-nodes 0)
            (collect-node-vector
             (shiftf current-node-vector
                     ensuing-node-vector
                     (make-array p :initial-element nil))))
          ;; If there are still some nodes left in the node vector after the last
          ;; iteration, add that node vector to the schedule, too.
          (unless (zerop current-nodes)
            (collect-node-vector current-node-vector))))
      (steps))))

#+(or) ;; Usage example:
(let* ((g (make-graph))
       (dims '(3 4))
       (a (make-array dims))
       (b (make-array dims))
       (c (make-array dims))
       (arrays (list a b c))
       (coords (apply #'alexandria:map-product #'list (mapcar #'alexandria:iota dims))))
  (setf coords (alexandria:shuffle coords))
  (loop for array in arrays for letter across "abcdefg" do
    (loop for (i j) in coords do
      (setf (aref array i j)
            (graph-ensure-node g (format nil "~C~D~D" letter i j)))))
  (loop for (src dst) on arrays while dst do
    (setf coords (alexandria:shuffle coords))
    (loop for (i j) in coords do
      (graph-add-edge g (aref src i j) (aref dst i j) 100)
      (loop for (di dj) in '((0 1) (0 -1) (1 0) (-1 0)) do
        (when (array-in-bounds-p src (+ i di) (+ j dj))
          (graph-add-edge g (aref src (+ i di) (+ j dj)) (aref dst i j) 1)))))
  (time
   (loop for p from 1 to 12
         collect
         (let ((schedule (graph-parallel-depth-first-schedule g p)))
           `(:length ,(length schedule)
             :coordinates-per-worker
             ,(loop for k below p
                    collect
                    (length
                     (remove-duplicates
                      (mapcan
                       (lambda (v)
                         (when (elt v k)
                           (list (subseq (elt v k) 1))))
                       schedule)
                      :test #'equal))))))))
