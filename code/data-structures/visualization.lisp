;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass <data-flow-graph> (<graph>) ())

(defmethod graphviz-successors ((purpose <data-flow-graph>) (node data-structure))
  (inputs node))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (node data-structure))
  (list :label (format nil "~A~%~A"
                       (class-name (class-of node))
                       (index-space node))
        :shape "box"
        :style "filled"))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (node application))
  (list :label (format nil "~A~%~A~%~A"
                       (class-name (class-of node))
                       (operator node)
                       (index-space node))
        :fillcolor "indianred1"))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (node reduction))
  (list :label (format nil "~A~%~A~%~A"
                      (class-name (class-of node))
                      (operator node)
                      (index-space node))
        :fillcolor "indianred3"))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (node reference))
  (list :label (format nil "~A~%~A~%~A"
                       (class-name (class-of node))
                       (transformation node)
                       (index-space node))
        :fillcolor "gray"))

(defmethod view ((object data-structure) &optional (purpose '<data-flow-graph>))
  (call-next-method object purpose))

(defmethod graphviz-draw-graph
    :around ((purpose <data-flow-graph>) graph-roots &optional stream)
  (let ((*print-case* :downcase))
    (call-next-method purpose graph-roots stream)))