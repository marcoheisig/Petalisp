;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defgeneric predecessors (node))

(defgeneric stream-draw-graph (node stream))

(defgeneric label (node))

(defparameter *graphviz-node-table* nil)

(defun id (node)
  (symbol-name (gethash node *graphviz-node-table*)))

(defun draw-graph (start-nodes filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph G {~%")
    (format stream "    node [shape = box, style=filled];~%")
    (let ((*graphviz-node-table* (make-hash-table :test #'eq)))
      (dolist (start-node start-nodes)
        (stream-draw-graph start-node stream)))
    (format stream "}~%")))

(defmethod stream-draw-graph :around (node stream)
  (when (null (gethash node *graphviz-node-table*))
    (setf (gethash node *graphviz-node-table*) (gensym "NODE"))
    (call-next-method)
    (loop for predecessor in (predecessors node)
          do (stream-draw-graph predecessor stream)
             (format stream "    ~a -> ~a;~%"
                     (id predecessor) (id node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Predecessors of Petalisp nodes

(defmethod predecessors ((node source)) nil)

(defmethod predecessors ((node application)) (objects node))

(defmethod predecessors ((node reduction)) (list (object node)))

(defmethod predecessors ((node repetition)) (list (object node)))

(defmethod predecessors ((node fusion)) (objects node))

(defmethod predecessors ((node reference)) (list (object node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for drawing individual nodes

(defmethod stream-draw-graph ((node application) stream)
  (format stream "    ~a [fillcolor = tomato, label = \"~a ~a\"];~%"
          (id node) (name (operator node)) (index-space node)))

(defmethod stream-draw-graph ((node reduction) stream)
  (format stream "    ~a [fillcolor = cornflowerblue, label = \"~a ~a\"];~%"
          (id node) (name (operator node)) (index-space node)))

(defmethod stream-draw-graph ((node fusion) stream)
  (format stream "    ~a [fillcolor = grey, label = \"fuse ~a\"];~%"
          (id node) (index-space node)))

(defmethod stream-draw-graph ((node reference) stream)
  (format stream "    ~a [fillcolor = lavender, label = \"~a ~a\"];~%"
          (id node) (transformation node) (index-space node)))

(defmethod stream-draw-graph ((node source) stream)
  (format stream "    ~a [fillcolor = cyan, label = \"~a\"];~%"
          (id node) (index-space node)))

(defmethod stream-draw-graph ((node strided-array-from-lisp-array) stream)
  (format stream "    ~a [fillcolor = cyan, label = \"~a\"];~%"
          (id node) (lisp-array node)))

(defmethod stream-draw-graph ((node repetition) stream)
  (format stream "    ~a [fillcolor = lightseagreen, label = \"~a\"];~%"
          (id node) (index-space node)))
