;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defmethod graphviz-incoming-edge-origins
    ((graph data-flow-graph)
     (edge data-flow-edge)
     (ir-node petalisp-ir:ir-node))
  (petalisp-ir:inputs ir-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp-ir:kernel))
  `(:fillcolor "gray"))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node petalisp-ir:buffer))
  `(:fillcolor "indianred1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (ir-node petalisp-ir:ir-node))
  `(("shape" . ,(stringify (petalisp-ir:shape ir-node)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (ir-node petalisp-ir:kernel))
  `(("body" . ,(stringify (subst-if
                           '*
                           (lambda (x)
                             (typep x 'petalisp-ir:buffer))
                           (petalisp-ir:body ir-node))))))
