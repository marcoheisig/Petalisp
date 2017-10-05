;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass data-flow-graph (<graph>) ())

(defmethod graphviz-graph-plist append-plist
    ((purpose data-flow-graph))
  `(:splines "ortho"))

(defmethod graphviz-successors
    ((purpose data-flow-graph) (node data-structure))
  (inputs node))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (node t))
  `(:shape "box" :style "filled"))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (node data-structure))
  `(:label ,(format nil "~A~%~A"
                    (class-name (class-of node))
                    (index-space node))))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (node application))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (operator node)
                    (index-space node))
    :fillcolor "indianred1"))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (node reduction))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (operator node)
                    (index-space node))
    :fillcolor "indianred3"))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (node reference))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (transformation node)
                    (index-space node))
    :fillcolor "gray"))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (node strided-array-constant))
  `(:label ,(let ((*print-right-margin* 60))
              (format nil "~A~%~A~%~A"
                      (class-name (class-of node))
                      (index-space node)
                      (let ((*print-length* 8))
                        (format nil "~A" (storage node)))))
    :fillcolor "gray55"))

(defmethod graphviz-edge-plist append-plist
    ((purpose data-flow-graph) (node-1 t) (node-2 t))
  `(:dir "back"))

;;; Graphviz plots of Petalisp data flow graphs are often obfuscated by a
;;; small number of constants with edges to almost every other node. So
;;; we define a subtype of graphs where constants are omitted.

(defclass simplified-data-flow-graph (data-flow-graph) ())

(defmethod graphviz-successors :around
    ((purpose simplified-data-flow-graph) (node t))
  (remove-if #'strided-array-constant? (call-next-method)))
