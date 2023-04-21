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
   #:graph
   #:graphp
   #:graph-initial-nodes
   #:graph-final-nodes
   #:graph-object-nodes
   #:graph-ensure-node
   #:graph-ensure-edge
   #:graph-parallel-depth-first-schedule))

(in-package #:petalisp.scheduling)

(defstruct (node
            (:predicate nodep)
            (:constructor make-node))
  ;; The nodes that this node immediately depends on.
  (predecessors '() :type list)
  ;; The nodes that depend on this node.
  (successors '() :type list)
  ;; The object that is managed by this node.
  (object nil :type t)
  ;; The distance of the longest path to a node with zero predecessors.
  (depth 0 :type unsigned-byte)
  ;; The distance of the longest path to a node with zero successors.
  (height 0 :type unsigned-byte)
  ;; A measure that is dynamically adapted during scheduling to prioritize
  ;; nodes that lead to good data locality.
  (eagerness 0 :type unsigned-byte)
  ;; A counter used for various purposes during scheduling.
  (counter 0 :type unsigned-byte)
  ;; This slot is used in depth-first scheduling to track the node's position
  ;; in the priority queue, and later in parallel depth-first scheduling to
  ;; track the node's position in the underlying depth-first schedule.
  (position nil))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~S" (node-object node))))

(defun copy-node-into (node other-node)
  (declare (node node other-node))
  (setf (node-predecessors other-node)
        (node-predecessors node))
  (setf (node-successors other-node)
        (node-successors node))
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
      ;; Otherwise, none of the nodes are equally important.
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

(defun graph-ensure-edge (graph from to)
  (declare (graph graph) (node from to))
  (declare (ignore graph))
  (assert (not (eq from to)))
  (pushnew from (node-predecessors to))
  (pushnew to (node-successors from)))

(defun graph-compute-node-height (graph)
  (graph-compute-distances
   graph
   #'node-height
   #'(setf node-height)
   #'node-successors
   #'node-predecessors))

(defun graph-compute-node-depth (graph)
  (graph-compute-distances
   graph
   #'node-depth
   #'(setf node-depth)
   #'node-predecessors
   #'node-successors))

(defun graph-compute-distances (graph get-distance set-distance inputs outputs)
  (flet ((node-distance (node)
           (funcall get-distance node))
         ((setf node-distance) (value node)
           (funcall set-distance value node)))
    (let ((initial-nodes '()))
      ;; Initialize the nodes and the worklist.
      (loop for node being the hash-values of (graph-object-nodes graph) do
        (let ((n (length (funcall inputs node))))
          (setf (node-counter node) n)
          (when (zerop n)
            (push node initial-nodes))))
      ;; Compute the distance to each node.
      (let ((worklist initial-nodes))
        (loop until (null worklist) for node = (pop worklist) do
          (setf (node-distance node)
                (1+ (reduce #'max (funcall inputs node)
                              :key #'node-distance
                              :initial-value 0)))
          (loop for output in (funcall outputs node) do
            ;; Decrement counter.
            (when (zerop (decf (node-counter output)))
              (push output worklist)))))
      initial-nodes)))

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
  (let ((queue (queues:make-queue :priority-queue :compare #'node-more-important-p))
        (tmpnode (make-node)))
    (flet ((push-node (node)
             (assert (null (node-position node)))
             (setf (node-position node)
                   (nth-value 1 (queues:qpush queue node)))
             node)
           (pop-node ()
             (let ((node (queues:qpop queue)))
               (setf (node-position node) nil)
               node))
           (increase-node-eagerness (node increment)
             ;; This function is complicated by the fact that we want to avoid
             ;; consing when updating the queue position.
             (if (not (node-position node))
                 (incf (node-eagerness node) increment)
                 (let ((position (node-position node)))
                   (copy-node-into node tmpnode)
                   (incf (node-eagerness tmpnode) increment)
                   (queues:queue-change queue position tmpnode)
                   (incf (node-eagerness node) increment)
                   (setf (queues::node-value position) node)))))
      ;; Initialize node counters and the queue.
      (loop for node being the hash-values of (graph-object-nodes graph) do
        (let ((n (length (node-predecessors node))))
          (setf (node-counter node) n)
          (when (zerop n)
            (push-node node))))
      ;; Generate the schedule.
      (loop until (zerop (queues:qsize queue))
            collect
            (let ((vector (make-array p :initial-element nil))
                  (end 0))
              ;; Grab up to P nodes form the queue.
              (loop repeat p until (zerop (queues:qsize queue)) do
                (let ((node (pop-node)))
                  ;; Increase the eagerness of each neighbor.
                  (loop for successor in (node-successors node) do
                    (unless (zerop (node-counter successor))
                      (loop for neighbor in (node-predecessors successor) do
                        (increase-node-eagerness neighbor 1))))
                  ;; TODO Insert the node at the position that maximizes memory
                  ;; locality with respect to earlier nodes.
                  (setf (svref vector end) node)
                  (incf end)))
              ;; Mark all nodes as completed and unwrap them.
              (loop for index below p do
                (let ((node (svref vector index)))
                  (when (nodep node)
                    (loop for successor in (node-successors node) do
                      (when (zerop (decf (node-counter successor)))
                        (queues:qpush queue successor)))
                    (setf (svref vector index)
                          (node-object node)))))
              vector)))))

;;;#+(or) ;; Usage example:
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
      (graph-ensure-edge g (aref src i j) (aref dst i j))
      (loop for (di dj) in '((0 1) (0 -1) (1 0) (-1 0)) do
        (when (array-in-bounds-p src (+ i di) (+ j dj))
          (graph-ensure-edge g (aref src (+ i di) (+ j dj)) (aref dst i j))))))
  (time
   (loop for p from 1 to 5 collect (graph-parallel-depth-first-schedule g p))))
