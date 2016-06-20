;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Serialize a graph in the graphviz dot format

(in-package :petalisp)

(defgeneric children (node))

(defgeneric stream-draw-graph (node stream))

(defgeneric label (node))

(defparameter *graphviz-node-table* nil)

(defun id (node)
  (symbol-name (gethash node *graphviz-node-table*)))

(defun draw-graph (graph filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph G {~%")
    (let ((*graphviz-node-table* (make-hash-table :test #'eq)))
      (stream-draw-graph graph stream))
    (format stream "}~%")))

(defmethod stream-draw-graph :around (node stream)
  (when (null (gethash node *graphviz-node-table*))
    (setf (gethash node *graphviz-node-table*) (gensym)))
  (format stream "  ~a [shape = box];~%"
          (id node))
  (call-next-method)
  (loop for child in (children node)
        for i from 1
        do (stream-draw-graph child stream)
           (format stream "  ~a -> ~a [label = \"~d\"];~%"
                   (id node) (id child) i)))
