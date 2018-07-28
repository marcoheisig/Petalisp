;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphviz visualization of data flow graphs

(defclass data-flow-graph (graph) ())

(defclass data-flow-edge (edge) ())

(defmethod graphviz-graph-attributes
    ((graph data-flow-graph))
  `(:splines "ortho"))

(defmethod graphviz-potential-edges append
    ((graph data-flow-graph)
     (node t))
  (list (make-instance 'data-flow-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph data-flow-graph)
     (edge data-flow-edge)
     (node data-structure))
  (inputs node))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node t))
  `(:shape :box :style :filled))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (data-structure data-structure))
  `(:label ,(format nil "~A~%~A"
                    (class-name (class-of data-structure))
                    (index-space data-structure))))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node strided-array-immediate))
  `(:fillcolor
    "cadetblue1"
    ,@(when-let ((storage (and (= 1 (size node))
                               (storage node))))
        (let ((*print-right-margin* 60))
          `(:label
            ,(format nil "~A~%~A~%~A"
                     (class-name (class-of node))
                     (index-space node)
                     (let ((*print-length* 8))
                       (format nil "~A" (storage node)))))))))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node application))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (operator node)
                    (index-space node))
    :fillcolor "burlywood1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node reduction))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (binary-operator node)
                    (index-space node))
    :fillcolor "beige"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node fusion))
  `(:fillcolor "cyan3"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node reference))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (transformation node)
                    (index-space node))
    :fillcolor "gray"))
