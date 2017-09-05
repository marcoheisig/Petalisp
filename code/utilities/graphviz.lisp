;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; A single data structure might be interpreted as many different
;;; graphs. To account for this, all subsequent generic functions accept a
;;; "purpose" object as their first argument, which should be an instance
;;; of a subclass of <graph>. The class <s-expression> and the specialized
;;; methods for it illustrate the general technique.
;;;
;;; Example:
;;; (graphviz-draw-graph '<s-expression> (list '(a b (c d)))) prints
;;;
;;; digraph G {
;;;   node1 [label="A"];
;;;   node2 [label="B"];
;;;   node3 [label="C"];
;;;   node4 [label="D"];
;;;   node1 -> node2 [label=""];
;;;   node1 -> node3 [label=""];
;;;   node3 -> node4 [label=""];
;;; }
;;;
;;; while
;;;
;;; (graphviz-draw-graph '<graph> (list '(a b (c d)))) prints
;;;
;;; digraph G {
;;;   node1 [label="(A B (C D))"];
;;; }.

(defclass <graph> () ())

(defclass <s-expression> (<graph>) ())

(defgeneric graphviz-successors (purpose node)
  (:method ((purpose <graph>) (node t)) nil)
  (:method ((purpose <s-expression>) (node list)) (cdr node)))

(defgeneric graphviz-node-label (purpose node)
  (:method ((purpose <graph>) (node t))
    (with-output-to-string (stream)
      (print-object node stream)))
  (:method ((purpose <s-expression>) (node list))
    (with-output-to-string (stream)
      (print-object (car node) stream))))

(defgeneric graphviz-node-color (purpose node)
  (:method ((purpose <graph>) (node t)) "white")
  (:method ((purpose <s-expression>) (node list)) "gray"))

(defgeneric graphviz-edge-label (purpose from to)
  (:method ((purpose <graph>) (from t) (to t)) ""))

(defgeneric graphviz-edge-color (purpose from to)
  (:method ((purpose <graph>) (from t) (to t)) "black"))

(defgeneric graphviz-draw-graph (purpose graph-roots &optional stream)
  (:method ((purpose symbol) (graph-roots list) &optional (stream t))
    (graphviz-draw-graph (make-instance purpose) graph-roots stream))
  (:method ((purpose <graph>) (graph-roots list) &optional (stream t))
    (let ((table (make-hash-table :test #'eq))
          (node-id 0))
      ;; 1. populate node table
      (labels ((populate-node-table (node)
                 (unless (gethash node table)
                   (setf (gethash node table) (incf node-id))
                   (dolist (successor (graphviz-successors purpose node))
                     (populate-node-table successor)))))
        (mapc #'populate-node-table graph-roots))
      (format stream "digraph G {~%")
      (format stream "  node[shape=box, style=filled]~%")
      ;; 2. draw nodes
      (loop :for node :being :each :hash-key
              :using (:hash-value node-id) :of table :do
                (format stream "  node~d [label=~S, fillcolor=~S]~%"
                        node-id
                        (graphviz-node-label purpose node)
                        (graphviz-node-color purpose node)))
      ;; 2. draw edges
      (loop :for from :being :each :hash-key
              :using (:hash-value from-id) :of table :do
                (dolist (to (graphviz-successors purpose from))
                  (let ((to-id (gethash to table)))
                    (format stream "  node~d -> node~d [label=~S, color=~S]~%"
                            from-id to-id
                            (graphviz-edge-label purpose from to)
                            (graphviz-edge-color purpose from to)))))
      (format stream "}~%"))))

#+nil
(defgeneric printworthy? (object)
  (:method ((object t)) t)
  (:method ((slot standard-effective-slot-definition))
    (not (member (slot-definition-name slot) '(ranges predecessors)))))

#+nil
(defmethod stream-draw-graph ((node standard-object) stream)
  (let ((class (class-of node)))
    (format stream "    ~a [label=\"" (id node))
    (format stream "~a\\n" (string-downcase (class-name class)))
    (loop for slot in (class-slots class)
          when (printworthy? slot) do
            (format
             stream "~a: ~a\\n"
             (string-downcase (slot-definition-name slot))
             (remove-if
              (lambda (x) (member x '(#\# #\< #\>)))
              (format nil "~a" (slot-value-using-class class node slot)))))
    (format stream "\"]~%")))
