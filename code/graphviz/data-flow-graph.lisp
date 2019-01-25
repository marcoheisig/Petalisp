;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass data-flow-graph (petalisp-graph)
  ())

(defclass data-flow-edge (petalisp-edge)
  ())

(defmethod graphviz-default-graph ((node petalisp.strided-arrays:strided-array))
  'data-flow-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connectivity

(defmethod graphviz-potential-edges append
    ((graph data-flow-graph)
     (node t))
  (list (make-instance 'data-flow-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph data-flow-graph)
     (edge data-flow-edge)
     (strided-array petalisp.strided-arrays:strided-array))
  (inputs strided-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate petalisp.strided-arrays:array-immediate))
  `(:fillcolor "cadetblue1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate petalisp.strided-arrays:application))
  `(:fillcolor "burlywood1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate petalisp.strided-arrays:reduction))
  `(:fillcolor "beige"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.strided-arrays.fusion))
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
     (strided-array petalisp.strided-arrays:strided-array))
  `(("shape" . ,(stringify (shape strided-array)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (array-immediate petalisp.strided-arrays:array-immediate))
  `(("storage" . ,(stringify (storage array-immediate)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (range-immediate range-immediate))
  `())

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.strided-arrays:application))
  `(("operator" . ,(stringify (operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.strided-arrays:reduction))
  `(("operator" . ,(stringify (operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.strided-arrays:reference))
  `(("transformation" . ,(stringify (transformation node)))))
