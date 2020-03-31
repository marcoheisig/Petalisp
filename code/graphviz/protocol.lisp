;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

;;; These first three generic functions compute Graphviz attributes for the
;;; entire graph, individual nodes, and individual edges, respectively.
;;; Each method on any of these three generic functions must return a
;;; property list of valid Graphviz attributes.
;;;
;;; The method combination GRAPHVIZ-ATTRIBUTES ensures that if a particular
;;; key appears in the property lists of several applicable methods, only
;;; the corresponding value in the property list of the most specific
;;; applicable method is used.  For example, consider the following
;;; definition of SOME-GRAPH:
;;;
;;; (defclass some-graph (graph) ())
;;;
;;; (defmethod graphviz-node-attributes
;;;     ((graph some-graph) (node t))
;;;   '(:style :filled
;;;     :fillcolor :white
;;;     :shape :box))
;;;
;;; In this setup, the following code defines a subclass of SOME-GRAPH,
;;; where nodes inherit the :STYLE and :SHAPE attribute, but where each
;;; node is colored red:
;;;
;;; (defclass red-graph (some-graph) ())
;;;
;;; (defmethod graphviz-node-attributes
;;;     ((graph red-graph) (node t))
;;;   '(:fillcolor :red))

(defgeneric graphviz-graph-attributes (graph)
  (:method-combination graphviz-attributes))

(defgeneric graphviz-node-attributes (graph node)
  (:method-combination graphviz-attributes))

(defgeneric graphviz-edge-attributes (graph edge from to edge-number)
  (:method-combination graphviz-attributes))

;;; The caption and properties of a node are used to compute its label.
;;; The caption of a node must be a string that is printed prominently in
;;; the first line of the node.  The properties of a node must be an alist,
;;; whose keys and values are strings. Each alist entry is rendered as a
;;; row below the caption.  Using APPEND as the method combination for node
;;; properties allows several applicable methods to contribute to the list
;;; of properties.
;;;
;;; For example, the following code could be used to display hypothetical
;;; person objects as nodes, using their full name as caption and metadata
;;; such as age and weight as rows below the caption:
;;;
;;; (defmethod graphviz-node-caption
;;;     ((graph some-graph) (person person))
;;;   (person-full-name person))
;;;
;;; (defmethod graphviz-node-properties
;;;     ((graph some-graph) (person person))
;;;   `(("age" . ,(princ-to-string (person-age person)))
;;;     ("weight" . ,(princ-to-string (person-weight person)))))
;;;
;;; Assuming a class FEMALE-PERSON that is a subclass of PERSION, we can
;;; register the additional information with the following method:
;;;
;;; (defmethod graphviz-node-properties
;;;     ((graph some-graph) (person female-person))
;;;   `(("gender" . "female")))

(defgeneric graphviz-node-caption (graph node))

(defgeneric graphviz-node-properties (graph node)
  (:method-combination append))

;;; The edge drawing protocol is somewhat involved, but for a good reason.
;;; We want to be able to subclass graphs to add or remove edges or to
;;; change the appearance of some edges.  To do so, the protocol works in
;;; three steps.
;;;
;;; 1. The generic function GRAPHVIZ-POTENTIAL-EDGES is used to determine
;;;    the set of edge types that potentially reach a node.
;;;
;;; 2. The generic functions GRAPHVIZ-OUTGOING-EDGE-TARGETS and
;;;    GRAPHVIZ-INCOMING-EDGE-ORIGINS are used to query for incoming and
;;;    outgoing edges of each of the previously determined edge types.
;;;
;;; 3. The current graph type, edge type, start node, target node and the
;;;    position of the edge in the sequence returned from the previous step
;;;    are used to derive the attributes of that edge.

(defgeneric graphviz-potential-edges (graph node)
  (:method-combination append))

(defgeneric graphviz-outgoing-edge-targets (graph edge node))

(defgeneric graphviz-incoming-edge-origins (graph edge node))

;;; The generic function GRAPHVIZ-KNOWN-NODES is necessary to describe
;;; graphs that are not fully connected.  It returns a list of nodes that
;;; are related to the current node, even though there is no edge that
;;; connects them.

(defgeneric graphviz-known-nodes (graph node)
  (:method-combination append))

;;; This generic function decides what kind of graph to use by default for
;;; a given node.

(defgeneric graphviz-default-graph (node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass any-graph ()
  ())

(defclass any-edge ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Methods

(defmethod graphviz-graph-attributes
    ((graph any-graph))
  '())

(defmethod graphviz-node-attributes
    ((graph any-graph) node)
  '())

(defmethod graphviz-edge-attributes
    ((graph any-graph) (edge any-edge) from to edge-number)
  '())

(defmethod graphviz-node-caption
    ((graph any-graph) node)
  (string-downcase
   (class-name
    (class-of node))))

(defmethod graphviz-node-properties append
    ((graph any-graph) node)
  '())

(defmethod graphviz-potential-edges append
    ((graph any-graph) node)
  '())

(defmethod graphviz-outgoing-edge-targets
    ((graph any-graph) (edge any-edge) node)
  '())

(defmethod graphviz-incoming-edge-origins
    ((graph any-graph) (edge any-edge) node)
  '())

(defmethod graphviz-known-nodes append
    ((graph any-graph) node)
  '())

(defmethod graphviz-default-graph :around
    ((node t))
  (let ((value (call-next-method)))
    (if (symbolp value)
        (make-instance value)
        value)))

(defmethod graphviz-default-graph
    ((node t))
  'any-graph)

(defmethod graphviz-default-graph
    ((list cons))
  (graphviz-default-graph (first list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CL-DOT Integration

(defmethod cl-dot:generate-graph-from-roots :around
    ((graph any-graph) objects &optional attributes)
  (let ((attributes (plist-union attributes (graphviz-graph-attributes graph))))
    (call-next-method graph objects attributes)))

(defmethod cl-dot:graph-object-node
    ((graph any-graph) node)
  (make-instance 'cl-dot:node
    :attributes
    `(:label
      ,(make-html-label
        :caption (graphviz-node-caption graph node)
        :properties (graphviz-node-properties graph node))
      ,@(graphviz-node-attributes graph node))))

(defmethod cl-dot:graph-object-points-to
    ((graph any-graph) node)
  (loop for edge in (graphviz-potential-edges graph node)
        append
        ;; There must be a more graceful way to loop over sequences...
        (loop for target in (coerce (graphviz-outgoing-edge-targets graph edge node) 'list)
              for edge-number from 0
              collect
              (make-instance 'cl-dot:attributed
                :attributes (graphviz-edge-attributes graph edge node target edge-number)
                :object target))))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph any-graph) node)
  (loop for edge in (graphviz-potential-edges graph node)
        append
        (loop for origin in (coerce (graphviz-incoming-edge-origins graph edge node) 'list)
              for edge-number from 0
              collect
              (make-instance 'cl-dot:attributed
                :attributes (graphviz-edge-attributes graph edge origin node edge-number)
                :object origin))))

(defmethod cl-dot:graph-object-knows-of
    ((graph any-graph) object)
  (graphviz-known-nodes graph object))
