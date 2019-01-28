;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass data-flow-graph (petalisp-graph)
  ())

(defclass data-flow-edge (petalisp-edge)
  ())

(defmethod graphviz-default-graph ((node petalisp.core:strided-array))
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
     (strided-array petalisp.core:strided-array))
  (petalisp.core:inputs strided-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate petalisp.core:array-immediate))
  `(:fillcolor "cadetblue1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate petalisp.core:application))
  `(:fillcolor "burlywood1"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (immediate petalisp.core:reduction))
  `(:fillcolor "beige"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:fusion))
  `(:fillcolor "cyan3"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp.core:reference))
  `(:fillcolor "gray"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (strided-array petalisp.core:strided-array))
  `(("shape" . ,(stringify (petalisp.core:shape strided-array)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (array-immediate petalisp.core:array-immediate))
  `(("storage" . ,(stringify (petalisp.core:storage array-immediate)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (range-immediate petalisp.core:range-immediate))
  `())

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.core:application))
  `(("operator" . ,(stringify (petalisp.core:operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.core:reduction))
  `(("operator" . ,(stringify (petalisp.core:operator node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (node petalisp.core:reference))
  `(("transformation" . ,(stringify (petalisp.core:transformation node)))))
