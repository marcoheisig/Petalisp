;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defmethod graphviz-incoming-edge-origins
    ((graph data-flow-graph)
     (edge data-flow-edge)
     (buffer petalisp-ir:buffer))
  (petalisp-ir:inputs buffer))

(defmethod graphviz-incoming-edge-origins
    ((graph data-flow-graph)
     (edge data-flow-edge)
     (kernel petalisp-ir:kernel))
  (mapcar #'car (petalisp-ir:loads kernel)))

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
     (buffer petalisp-ir:buffer))
  `(("shape" . ,(stringify (petalisp-ir:shape buffer)))))

(defmethod graphviz-node-properties append
    ((graph data-flow-graph)
     (kernel petalisp-ir:kernel))
  `(("body" . "TODO")))
