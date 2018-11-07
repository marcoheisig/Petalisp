;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-graph (petalisp-graph) ())

(defclass ir-edge (petalisp-edge) ())

(defclass load-edge (ir-edge) ())

(defclass store-edge (ir-edge) ())

(defclass input-edge (ir-edge) ())

(defclass output-edge (ir-edge) ())

(defmethod graphviz-default-graph ((node petalisp-ir:kernel))
  'ir-graph)

(defmethod graphviz-default-graph ((node petalisp-ir:buffer))
  'ir-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connectivity

(defmethod graphviz-potential-edges append
    ((graph ir-graph) node)
  (list (make-instance 'load-edge)
        (make-instance 'store-edge)
        (make-instance 'input-edge)
        (make-instance 'output-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph ir-graph)
     (edge input-edge)
     (buffer petalisp-ir:buffer))
  (petalisp-ir:inputs buffer))

(defmethod graphviz-outgoing-edge-targets
    ((graph ir-graph)
     (edge output-edge)
     (buffer petalisp-ir:buffer))
  (petalisp-ir:outputs buffer))

(defmethod graphviz-incoming-edge-origins
    ((graph ir-graph)
     (edge load-edge)
     (kernel petalisp-ir:kernel))
  (mapcar #'car (petalisp-ir:loads kernel)))

(defmethod graphviz-outgoing-edge-targets
    ((graph ir-graph)
     (edge store-edge)
     (kernel petalisp-ir:kernel))
  (mapcar #'car (petalisp-ir:stores kernel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edge Appearance

(defmethod graphviz-edge-attributes
    ((graph ir-graph) (edge input-edge) from to edge-number)
  `(:color "orange"))

(defmethod graphviz-edge-attributes
    ((graph ir-graph) (edge output-edge) from to edge-number)
  `(:color "green"))

(defmethod graphviz-edge-attributes
    ((graph ir-graph) (edge load-edge) from to edge-number)
  `(:color "blue"))

(defmethod graphviz-edge-attributes
    ((graph ir-graph) (edge store-edge) from to edge-number)
  `(:color "red"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph ir-graph)
     (node petalisp-ir:kernel))
  `(:fillcolor "gray"))

(defmethod graphviz-node-attributes
    ((graph ir-graph)
     (node petalisp-ir:buffer))
  `(:fillcolor "indianred1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph ir-graph)
     (buffer petalisp-ir:buffer))
  `(("shape" . ,(stringify (petalisp-ir:buffer-shape buffer)))))

(defun hide-buffers (references)
  (subst-if :buffer (lambda (x) (typep x 'petalisp-ir:buffer)) references))

(defmethod graphviz-node-properties append
    ((graph ir-graph)
     (kernel petalisp-ir:kernel))
  `(("body" . "TODO")))
