;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; A sequence of root objects can be interpreted as many different
;;; graphs. To account for this, all subsequent generic functions accept a
;;; "purpose" object as their first argument, which should be an instance
;;; of a subclass of <graph>.

(defclass <graph> () ())

(defgeneric graphviz-successors (purpose node)
  (:method ((purpose <graph>) (node t)) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun plist-append (&rest plists)
    (let (result)
      (loop :for (key-1 . rest) :on (apply #'append plists) :by #'cddr :do
        (unless (loop :for (key-2 . nil) :on result :by #'cddr
                        :thereis (eq key-1 key-2))
          (push (car rest) result)
          (push key-1 result)))
      result))
  (define-method-combination plist-append :identity-with-one-argument t))

(defgeneric graphviz-graph-plist (purpose)
  (:method-combination plist-append)
  (:method plist-append ((purpose <graph>))
    (list)))

(defgeneric graphviz-node-plist (purpose node)
  (:method-combination plist-append)
  (:method plist-append ((purpose <graph>) (node t))
    (list :label (with-output-to-string (stream)
                   (print-object node stream)))))

(defgeneric graphviz-edge-plist (purpose from to)
  (:method-combination plist-append)
  (:method plist-append ((purpose <graph>) (from t) (to t))
    (list :color "black")))

(defgeneric graphviz-draw-graph (purpose graph-roots &optional stream)
  (:method ((purpose symbol) (graph-roots list) &optional (stream t))
    (graphviz-draw-graph (make-instance purpose) graph-roots stream))
  (:method ((purpose <graph>) (graph-roots list) &optional (stream t))
    (let ((table (make-hash-table :test #'eq))
          (node-id 0))
      ;; 1. populate node table
      (labels ((populate-node-table (node)
                 (unless (gethash node table)
                   (setf (gethash node table) (incf node-id))
                   (dolist (successor (graphviz-successors purpose node))
                     (populate-node-table successor)))))
        (map nil #'populate-node-table graph-roots))
      (format stream "digraph G {~%")
      ;; 2. write graph attributes
      (format stream "~{  ~A=~S~%~}" (graphviz-graph-plist purpose))
      ;; 3. write nodes
      (loop :for node :being :each :hash-key
              :using (:hash-value node-id) :of table :do
                (format stream "  node~d [~{~A=~S,~^ ~}]~%"
                        node-id (graphviz-node-plist purpose node)))
      ;; 4. write edges
      (loop :for from :being :each :hash-key
              :using (:hash-value from-id) :of table :do
                (dolist (to (graphviz-successors purpose from))
                  (let ((to-id (gethash to table)))
                    (format stream "  node~d -> node~d [~{~A=~S,~^ ~}]~%"
                            from-id to-id
                            (graphviz-edge-plist purpose from to)))))
      (format stream "}~%"))))
