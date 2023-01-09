;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass petalisp-graph (any-graph)
  ())

(defclass petalisp-edge (any-edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-node-attributes
    ((graph petalisp-graph)
     (node t))
  `(:shape :box :style :filled :penwidth 2.0))

(defmethod graphviz-graph-attributes
    ((graph petalisp-graph))
  `())

(defmethod graphviz-edge-attributes
    ((graph petalisp-graph)
     (edge petalisp-edge)
     (form t)
     (to t)
     edge-number)
  '(:penwidth 2.0))
