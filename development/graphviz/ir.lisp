;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defmethod graphviz-incoming-edge-origins
    ((graph data-flow-graph)
     (edge data-flow-edge)
     (ir-node ir-node))
  (inputs ir-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node kernel))
  `(:fillcolor "gray"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node buffer))
  `(:fillcolor "indianred1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (ir-node ir-node))
  `(("shape" . ,(stringify (shape ir-node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (ir-node kernel))
  `(("body" . ,(stringify (subst-if '* #'bufferp (body ir-node))))))
