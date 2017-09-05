;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass <data-flow-graph> (<graph>) ())

(defmethod graphviz-successors ((purpose <data-flow-graph>) (node data-structure))
  (predecessors node))

(defmethod graphviz-node-label ((purpose <data-flow-graph>) (node data-structure))
  (format nil "~A~%~A"
          (class-name (class-of node))
          (index-space node)))

(defmethod graphviz-node-label ((purpose <data-flow-graph>) (node application))
  (format nil "~A~%~A~%~A"
          (class-name (class-of node))
          (operator node)
          (index-space node)))

(defmethod graphviz-node-label ((purpose <data-flow-graph>) (node reduction))
  (format nil "~A~%~A~%~A"
          (class-name (class-of node))
          (operator node)
          (index-space node)))

(defmethod graphviz-node-label ((purpose <data-flow-graph>) (node reference))
  (format nil "~A~%~A~%~A"
          (class-name (class-of node))
          (transformation node)
          (index-space node)))

(defmethod graphviz-node-label ((purpose <data-flow-graph>) (node elaboration))
  (format nil "~A~%~A~%~A"
          (class-name (class-of node))
          (data node)
          (index-space node)))

(defmethod graphviz-node-color ((purpose <data-flow-graph>) (node application))
  (values "indianred1"))

(defmethod graphviz-node-color ((purpose <data-flow-graph>) (node reduction))
  (values "indianred3"))

(defmethod graphviz-node-color ((purpose <data-flow-graph>) (node reference))
  (values "gray"))
