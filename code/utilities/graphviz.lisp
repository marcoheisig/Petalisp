;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append-plist (&rest plists)
    "Non-destructively merge the given PLISTS. If a key occurs more than
    once, the leftmost value takes precedence."
    (let (result)
      (loop :for (key-1 . rest) :on (apply #'append plists) :by #'cddr :do
        (unless (loop :for (key-2 . nil) :on result :by #'cddr
                        :thereis (eq key-1 key-2))
          (push (car rest) result)
          (push key-1 result)))
      result))
  (define-method-combination append-plist :identity-with-one-argument t))

;;; A sequence of root objects can be interpreted as many different
;;; graphs. To account for this, all subsequent generic functions accept a
;;; "purpose" object as their first argument, which should be an instance
;;; of a subclass of graph. For a simple example on how to add new types of
;;; graphs, see the bottom of this file.

(defclass graph () ())

(defgeneric graphviz-successors (purpose node)
  (:method ((purpose graph) (node t)) nil))

(defgeneric graphviz-graph-plist (purpose)
  (:method-combination append-plist)
  (:method append-plist ((purpose graph)) nil))

(defgeneric graphviz-node-plist (purpose node)
  (:method-combination append-plist)
  (:method append-plist ((purpose graph) (node t))
    (list :label (with-output-to-string (stream)
                   (print-object node stream)))))

(defgeneric graphviz-edge-plist (purpose from to)
  (:method-combination append-plist)
  (:method append-plist ((purpose graph) (from t) (to t))
    (list :color "black")))

(defparameter *graphviz-pdf-viewer* "evince")

(defgeneric graphviz-draw-graph (purpose graph-roots &optional stream)
  (:documentation
   "Write object to STREAM in a format suitable for the program
   Graphviz (www.graphviz.org). If no stream is specified, write the graph
   to a temporary file and open it with *GRAPHVIZ-PDF-VIEWER*.

   The exact behavior of this method is governed by PURPOSE, which is an
   instance of a subclass of graph (or a symbol, denoting such an
   instance), and the generic functions GRAPHVIZ-SUCCESSORS,
   GRAPHVIZ-GRAPH-PLIST, GRAPHVIZ-NODE-PLIST and GRAPHVIZ-EDGE-PLIST.")

  ;; handle the case where STREAM is NIL
  (:method :around (purpose graph-roots &optional stream)
    (if stream
        (call-next-method)
        (with-temporary-file (:stream stream :pathname dotfile :direction :output)
          (call-next-method purpose graph-roots stream)
          :close-stream
          (with-temporary-file (:pathname imagefile)
            (run-program (list "dot" "-Tpdf" "-o"
                               (native-namestring imagefile)
                               (native-namestring dotfile)))
            (run-program (list *graphviz-pdf-viewer* (native-namestring imagefile))))))
    (values))

  ;; the actual graph drawing algorithm
  (:method (purpose-designator graph-roots &optional stream)
    (let ((table (make-hash-table :test #'eq))
          (node-counter 0)
          (*print-case* :downcase)
          (purpose (if (symbolp purpose-designator)
                       (make-instance purpose-designator)
                       purpose-designator)))
      ;; 1. populate node table
      (labels ((populate-node-table (node)
                 (unless (gethash node table)
                   (setf (gethash node table) (incf node-counter))
                   (map nil #'populate-node-table (graphviz-successors purpose node)))))
        (mapc #'populate-node-table
              (if (typep graph-roots 'sequence)
                  graph-roots
                  (list graph-roots))))
      (format stream "digraph G {~%")
      ;; 2. write graph attributes
      (format stream "~{  ~A=~S~%~}" (graphviz-graph-plist purpose))
      ;; 3. write nodes
      (loop :for node :being :each :hash-key
              :using (:hash-value node-id) :of table :do
                (format stream "  node~d [~{~A=~S~^, ~}]~%"
                        node-id (graphviz-node-plist purpose node)))
      ;; 4. write edges
      (loop :for from :being :each :hash-key
              :using (:hash-value from-id) :of table :do
                (map nil (lambda (to)
                           (let ((to-id (gethash to table)))
                             (format stream "  node~d -> node~d [~{~A=~S~^, ~}]~%"
                                     from-id to-id
                                     (graphviz-edge-plist purpose from to))))
                     (graphviz-successors purpose from)))
      (format stream "}~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; example: draw the class hierarchy of the current lisp image

(defclass class-hierarchy (graph) ())

(defmethod graphviz-successors ((purpose class-hierarchy) (node class))
  ;; Show only classes that are accessible in the current package and all
  ;; their superclasses
  (labels ((visible-class? (class)
             (or (when-let ((name (class-name class)))
                   (eq (find-symbol (symbol-name name)) name))
                 (some #'visible-class? (class-direct-subclasses class)))))
    (remove-if-not #'visible-class? (class-direct-subclasses node))))

(defmethod graphviz-node-plist append-plist ((purpose class-hierarchy) (node class))
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
