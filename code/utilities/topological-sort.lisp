;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun topological-sort (start-nodes map-node-successors map-node-predecessors)
  "Returns a list of all nodes in supplied DAG, in topological ordering.
The graph is specified by a list of start nodes, a function for applying a
supplied function to each successor of a supplied node, and a function for
applying a supplied function to each predecessor of a supplied node."
  (declare (list start-nodes)
           (function map-node-successors map-node-predecessors))
  (macrolet ((do-node-successors ((var node) &body body)
               `(funcall map-node-successors (lambda (,var) ,@body) ,node))
             (do-node-predecessors ((var node) &body body)
               `(funcall map-node-predecessors (lambda (,var) ,@body) ,node)))
    (dolist (start-node start-nodes)
      (do-node-predecessors (predecessor start-node)
        (error "Start nodes ~S has a predecessor (~S)."
               start-node predecessor)))
    (let ((removed (make-hash-table))
          (worklist start-nodes)
          (result '()))
      (loop until (null worklist) for node = (pop worklist) do
        (setf (gethash node removed) t)
        (push node result)
        (do-node-successors (successor node)
          (block abort
            (do-node-predecessors (predecessor successor)
              (unless (gethash predecessor removed)
                (return-from abort)))
            (push successor worklist))))
      (nreverse result))))
