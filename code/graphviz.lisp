;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; export Petalisp data flow problems as graphviz files

(in-package :petalisp)

(defgeneric stream-draw-graph (node stream))

(defparameter *graphviz-node-table* nil)

(defun id (node)
  (symbol-name (gethash node *graphviz-node-table*)))

(defun draw-graph (filename &rest nodes)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let ((*graphviz-node-table* (make-hash-table :test #'eq))
          (*print-pretty* nil))
      (format stream "digraph G {~%")
      (format stream "    node [shape=Mrecord];~%")
      (dolist (node nodes)
        (stream-draw-graph node stream))
      (format stream "}~%"))))

(defmethod stream-draw-graph :around (node stream)
  (when (null (gethash node *graphviz-node-table*))
    (setf (gethash node *graphviz-node-table*) (gensym "NODE"))
    (call-next-method)
    (loop for predecessor in (predecessors node)
          do (stream-draw-graph predecessor stream)
             (format stream "    ~a -> ~a;~%"
                     (id predecessor) (id node)))))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; methods to draw individual nodes
;;; _________________________________________________________________

(defmethod stream-draw-graph ((node application) stream)
  (format stream "    ~a [label=\"~w\\napplication ~w\"];~%"
          (id node) (index-space node) (name (operator node))))

(defmethod stream-draw-graph ((node reduction) stream)
  (format stream "    ~a [label = \"~w\\nreduction ~w\"];~%"
          (id node) (index-space node) (name (operator node))))

(defmethod stream-draw-graph ((node fusion) stream)
  (format stream "    ~a [label = \"fusion\\n~w\"];~%"
          (id node) (index-space node)))

(defmethod stream-draw-graph ((node reference) stream)
  (format stream "    ~a [label=\"~w\\n~w\"];~%"
          (id node)
          (transform (index-space node) (invert (transformation node)))
          (transformation node)))

(defmethod stream-draw-graph ((node repetition) stream)
  (format stream "    ~a [label = \"~w\\n~w\"];~%"
          (id node) (index-space (first (predecessors node))) (index-space node)))

(defmethod stream-draw-graph ((node data-structure) stream)
  (format stream "    ~a [label = \"~w\\n~w\"];~%"
          (id node) (class-name (class-of node))
          (index-space node)))

