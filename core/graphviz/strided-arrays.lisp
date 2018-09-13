;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defclass data-flow-graph (any-graph) ())

(defclass data-flow-edge (any-edge) ())

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
     (strided-array strided-array))
  (inputs strided-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node t))
  `(:shape :box :style :filled))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate scalar-immediate))
  `(:fillcolor "cadetblue1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate array-immediate))
  `(:fillcolor "cadetblue1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate application))
  `(:fillcolor "burlywood1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate reduction))
  `(:fillcolor "beige"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node fusion))
  `(:fillcolor "cyan3"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node reference))
  `(:fillcolor "gray"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (strided-array strided-array))
  `(("shape" . ,(stringify (shape strided-array)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (scalar-immediate scalar-immediate))
  `(("storage" . ,(stringify (storage scalar-immediate)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (array-immediate array-immediate))
  `(("storage" . ,(stringify (storage array-immediate)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node application))
  `(("operator" . ,(stringify (operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node reduction))
  `(("operator" . ,(stringify (operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node reference))
  `(("transformation" . ,(stringify (transformation node)))))
