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
   #:graph-depth-first-schedule
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
      ;; The next criterion is to prioritize nodes with fewer successors.  The
      ;; effect is that when a homogeneous array is partitioned into multiple
      ;; scheduling nodes, the processing starts at one of the boundaries.
      (cmp (length (node-successors node2))
           (length (node-successors node1)))
      ;; The final criterion is to prioritize nodes with more predecessors,
      ;; hoping that doing so will free more memory, sooner.
      (cmp (length (node-predecessors node1))
           (length (node-predecessors node2)))
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

(defun graph-depth-first-schedule (graph &key (finalize-node #'node-object))
  "Returns a list of finalized nodes in a depth-first order, where each node
appears later than all its predecessors."
  (declare (graph graph) (function finalize-node))
  (graph-compute-node-depth graph)
  (graph-compute-node-height graph)
  ;; Maintain a priority queue of all nodes that are ready to be scheduled.
  (let ((queue (queues:make-queue :priority-queue :compare #'node-more-important-p)))
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
             (prog1 (incf (node-eagerness node) increment)
               (when (node-position node)
                 ;; This deletion is the reason why we track the node's queue
                 ;; position in the first place:
                 (queues:queue-delete queue (node-position node))
                 (setf (node-position node)
                       (nth-value 1 (queues:qpush queue node)))))))
      ;; Initialize node counters and the queue.
      (loop for node being the hash-values of (graph-object-nodes graph) do
        (let ((n (length (node-predecessors node))))
          (setf (node-counter node) n)
          (when (zerop n)
            (push-node node))))
      ;; Generate the schedule.
      (loop until (zerop (queues:qsize queue))
            collect
            (let ((node (pop-node)))
              ;; Check with successors become ready once this node has been
              ;; scheduled.
              (loop for successor in (node-successors node) do
                (if (zerop (decf (node-counter successor)))
                    (push-node successor)
                    ;; If the node's successor is not ready, yet, increase the
                    ;; eagerness of each predecessor thereof.
                    (loop for neighbor in (node-predecessors successor) do
                      (increase-node-eagerness neighbor 1))))
              (funcall finalize-node node))))))

(defun node-lower-position-p (node1 node2)
  (< (node-position node1)
     (node-position node2)))

(defun graph-parallel-depth-first-schedule (graph p &key (finalize-node #'node-object))
  "Returns a list of vectors of length P whose entries are either NIL, or finalized nodes.
Each list entry describes the actions to be taken, possibly in parallel, in
each step of the schedule.  The K-th vector entry describes the action to be
taken by the K-th worker.

The schedule is computed using the P-DFS algorithm from Blelloch, Gibbons, and
Matias (https://doi.org/10.1145/301970.301974), with some augmentations to
improve data locality."
  (declare (graph graph) (unsigned-byte p) (function finalize-node))
  (let ((dfs (graph-depth-first-schedule graph :finalize-node #'identity))
        (queue (queues:make-queue :priority-queue :compare #'node-lower-position-p)))
    ;; Initialize each node's counter and position and fill the queue.
    (loop for node in dfs for position from 0 do
      (let ((n (length (node-predecessors node))))
        (setf (node-counter node) n)
        (setf (node-position node) position)
        (when (zerop n)
          (queues:qpush queue node))))
    ;; Generate the schedule.
    (loop until (zerop (queues:qsize queue))
          collect
          (let ((vector (make-array p :initial-element nil)))
            (loop until (zerop (queues:qsize queue)) for index below p do
              (setf (svref vector index)
                    (queues:qpop queue)))
            (loop for node across vector for index from 0 until (not node) do
              (setf (svref vector index)
                    (funcall finalize-node node))
              (loop for successor in (node-successors node) do
                (when (zerop (decf (node-counter successor)))
                  (queues:qpush queue successor))))
            vector))))

#+(or) ;; Usage example:
(let* ((g (make-graph))
       (a (make-array '(3 4)))
       (b (make-array '(3 4)))
       (c (make-array '(3 4)))
       (l (list a b c)))
  (loop for array in l for letter across "abcdefg" do
    (loop for i below (array-dimension array 0) do
      (loop for j below (array-dimension array 1) do
        (setf (aref array i j)
              (graph-ensure-node g (format nil "~C~D~D" letter i j))))))
  (loop for (src dst) on (list a b c) while dst do
    (loop for i below (array-dimension src 0) do
      (loop for j below (array-dimension src 1) do
        (graph-ensure-edge g (aref src i j) (aref dst i j))
        (loop for (di dj) in '((0 1) (0 -1) (1 0) (-1 0)) do
          (when (array-in-bounds-p src (+ i di) (+ j dj))
            (graph-ensure-edge g (aref src (+ i di) (+ j dj)) (aref dst i j)))))))
  (values l (graph-depth-first-schedule g)))
