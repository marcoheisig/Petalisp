;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass scheduling-graph (petalisp-graph) ())

(defclass scheduling-edge (petalisp-edge) ())

(defmethod graphviz-default-graph ((node petalisp.scheduling:node))
  'scheduling-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connectivity

(defmethod graphviz-potential-edges append
    ((graph scheduling-graph) node)
  (list (make-instance 'scheduling-edge)))

(defmethod graphviz-known-nodes append
    ((graph scheduling-graph)
     (node petalisp.scheduling:node))
  (mapcar #'car (petalisp.scheduling:node-predecessor-alist node)))

(defmethod graphviz-outgoing-edge-targets
    ((graph scheduling-graph)
     (edge scheduling-edge)
     (node petalisp.scheduling:node))
  (mapcar #'car (petalisp.scheduling:node-successor-alist node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph scheduling-graph)
     (node petalisp.scheduling:node))
  (let ((action (petalisp.scheduling::node-object node)))
    `(("copies" . ,(stringify
                    (mapcar #'petalisp.native-backend::invocation-iteration-space
                            (petalisp.native-backend::action-copy-invocations action))))
      ("work" . ,(stringify
                    (mapcar #'petalisp.native-backend::invocation-iteration-space
                            (petalisp.native-backend::action-work-invocations action)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edge Labels

(defmethod graphviz-edge-attributes
    ((graph scheduling-graph)
     (edge scheduling-edge)
     (from petalisp.scheduling:node)
     (to petalisp.scheduling:node)
     (n integer))
  `(:label ,(stringify (cdr (nth n (petalisp.scheduling:node-successor-alist from))))))
