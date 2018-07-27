;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun plist-union (&rest plists)
    "Return a plist containing each key that appears in any of the supplied
    PLISTS and its corresponding value. If a key occurs multiple times,
    only the corresponding value of the leftmost entry is used and all
    others are discarded."
    (declare (dynamic-extent plists))
    (let (result)
      (dolist (plist plists (nreverse result))
        (doplist (key-1 val-1 plist)
          (unless (loop for rest on (cdr result) by #'cddr
                          thereis (eq key-1 (car rest)))
            (push key-1 result)
            (push val-1 result))))))
  (define-method-combination plist-union))

;;; A sequence of root objects can be interpreted as many different
;;; graphs. To account for this, all subsequent generic functions accept a
;;; "purpose" object as their first argument, which should be an instance
;;; of a subclass of graphviz-graph. For a simple example on how to add new
;;; types of graphs, see the bottom of this file.

(defclass graphviz-graph () ())

(defgeneric graphviz-successors (purpose node)
  (:method ((purpose graphviz-graph) (node t)) nil))

(defgeneric graphviz-graph-plist (purpose)
  (:method-combination plist-union)
  (:method plist-union ((purpose graphviz-graph)) nil))

(defgeneric graphviz-node-plist (purpose node)
  (:method-combination plist-union)
  (:method plist-union ((purpose graphviz-graph) (node t))
    (list :label (with-output-to-string (stream)
                   (print-object node stream)))))

(defgeneric graphviz-edge-plist (purpose from to)
  (:method-combination plist-union)
  (:method plist-union ((purpose graphviz-graph) (from t) (to t)) nil))

(defgeneric graphviz-draw-graph (purpose graph-roots &optional stream)
  (:documentation
   "Write object to STREAM in a format suitable for the program
   Graphviz (www.graphviz.org). If no stream is specified, write the graph
   to a temporary file and open it with *GRAPHVIZ-PDF-VIEWER*.

   The exact behavior of this method is governed by PURPOSE, which is an
   instance of a subclass of graphviz-graph (or a symbol, denoting such an
   instance), and the generic functions GRAPHVIZ-SUCCESSORS,
   GRAPHVIZ-GRAPH-PLIST, GRAPHVIZ-NODE-PLIST and GRAPHVIZ-EDGE-PLIST.")
  (:method (purpose graph-roots &optional stream)
    (let ((table (make-hash-table :test #'eq))
          (*print-case* :downcase)
          (node-counter 0))
      (labels
          ((populate-node-table (node)
             (unless (gethash node table)
               (setf (gethash node table) (incf node-counter))
               (map nil #'populate-node-table (graphviz-successors purpose node))))
           (write-node (node id)
             (format stream "  node~d [~{~A=~S~^, ~}]~%" id
                     (graphviz-node-plist purpose node)))
           (write-edges (from from-id)
             (map nil
                  (lambda (to)
                    (let ((to-id (gethash to table)))
                      (format stream "  node~d -> node~d [~{~A=~S~^, ~}]~%"
                              from-id to-id
                              (graphviz-edge-plist purpose from to))))
                  (graphviz-successors purpose from))))
        (map nil #'populate-node-table graph-roots)
        (format stream "digraph G {~%")
        (format stream "~{  ~A=~S~%~}" (graphviz-graph-plist purpose))
        (maphash #'write-node table)
        (maphash #'write-edges table)
        (format stream "}~%")))))

(defparameter *graphviz-viewer* "evince")

(defparameter *graphviz-output-format* "pdf")

(defun graphviz (purpose-designator &rest graph-roots)
  (let ((purpose
          (typecase purpose-designator
            (symbol (make-instance purpose-designator))
            (t purpose-designator))))
    (with-temporary-file (:stream stream :pathname dotfile :direction :output)
      (graphviz-draw-graph purpose graph-roots stream)
      (finish-output stream)
      :close-stream
      (with-temporary-file (:pathname imagefile)
        (run-program (list "dot"
                           (format nil "-T~A" *graphviz-output-format*)
                           "-o"
                           (native-namestring imagefile)
                           (native-namestring dotfile)))
        (run-program (list *graphviz-viewer* (native-namestring imagefile)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Example 1: Draw the Class Hierarchy of the Current Lisp Image

(defclass class-hierarchy (graphviz-graph) ())

(defmethod graphviz-successors ((purpose class-hierarchy) (node class))
  ;; Show only classes that are accessible in the current package and all
  ;; their superclasses
  (labels ((visible-class? (class)
             (or (when-let ((name (class-name class)))
                   (eq (find-symbol (symbol-name name)) name))
                 (some #'visible-class? (class-direct-subclasses class)))))
    (remove-if-not #'visible-class? (class-direct-subclasses node))))

(defmethod graphviz-node-plist plist-union ((purpose class-hierarchy) (node class))
  `(:label ,(string (class-name node))
    ,@(when-let ((name (class-name node)))
        (cond
          ((eq (symbol-package name) *package*)
           `(:style "filled" :fillcolor "aquamarine" :shape "note"))
          ((subtypep name 'error)
           `(:style "filled" :fillcolor "lightcoral"))
          ((subtypep name 'warning)
           `(:style "filled" :fillcolor "lightsalmon"))
          ((subtypep name 'condition)
           `(:style "filled" :fillcolor "lavender"))))
    :shape "box"))
