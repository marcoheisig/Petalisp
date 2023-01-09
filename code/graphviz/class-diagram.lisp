;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass class-diagram (any-graph)
  ())

(defclass direct-subclass-edge (any-edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connectivity

(defmethod graphviz-potential-edges append
    ((graph class-diagram)
     (class class))
  (list (make-instance 'direct-subclass-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph class-diagram)
     (edge direct-subclass-edge)
     (class class))
  (closer-mop:class-direct-subclasses class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graph Appearance

(defmethod graphviz-graph-attributes
    ((graph class-diagram))
  `(:rankdir "BT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod cl-dot:graph-object-node
    ((graph class-diagram)
     (class class))
  (make-instance 'cl-dot:node
    :attributes
    `(:shape :record
      :style :filled
      :fillcolor "gray95"
      :label
      (:html
       ()
       (:table
        ((:border "0") (:cellborder "0") (:cellspacing "0"))
        (:tr () (:td ((:colspan "2") (:align "center")) (:b () ,(stringify (class-name class)))))
        ,@(loop for slot in (closer-mop:class-direct-slots class)
                collect
                `(:tr ()
                      (:td ((:align "left")) ,(format nil "~A :" (closer-mop:slot-definition-name slot)))
                      (:td ((:align "left")) ,(stringify (closer-mop:slot-definition-type slot))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edge Appearance

(defmethod graphviz-edge-attributes
    ((graph class-diagram)
     (edge direct-subclass-edge)
     (from class)
     (to class)
     edge-number)
  `(:arrowhead :onormal :style :dashed))
