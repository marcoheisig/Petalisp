;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass data-flow-graph (petalisp-graph)
  ())

(defclass data-flow-edge (petalisp-edge)
  ())

(defmethod graphviz-default-graph ((node petalisp.core:lazy-array))
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
     (lazy-array petalisp.core:lazy-array))
  (petalisp.core:lazy-array-inputs lazy-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:lazy-array))
  (graphviz-node-attributes graph (petalisp.core:lazy-array-delayed-action node)))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:delayed-array))
  `(:fillcolor "#ABCEE3" :style (:filled :diagonals)))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:delayed-map))
  `(:fillcolor "#B2DF8A"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:delayed-fuse))
  `(:fillcolor "#1F78B3"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:delayed-reshape))
  `(:fillcolor "#ABCEE3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Captions

(defmethod graphviz-node-caption
    ((graph data-flow-graph)
     (node petalisp.core:lazy-array))
  (graphviz-node-caption graph (petalisp.core:lazy-array-delayed-action node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.core:lazy-array))
  `(("shape" . ,(stringify (petalisp.core:lazy-array-shape node)))
    ("element-type" . ,(stringify (petalisp.core:lazy-array-element-type node)))
    ,@(graphviz-node-properties graph (petalisp.core:lazy-array-delayed-action node))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (delayed-array petalisp.core:delayed-array))
  `(("storage" . ,(stringify (petalisp.core:delayed-array-storage delayed-array)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.core:delayed-map))
  `(("operator" . ,(stringify (petalisp.core:delayed-map-operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.core:delayed-reshape))
  `(("transformation" . ,(stringify (petalisp.core:delayed-reshape-transformation node)))))
