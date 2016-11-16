;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; export Petalisp data flow problems as graphviz files

(in-package :petalisp)

(defgeneric stream-draw-graph (node stream))

(defparameter *graphviz-node-table* nil)

(defun id (node)
  (symbol-name (gethash node *graphviz-node-table*)))

(defun draw-graph (start-nodes filename)
  (unless (listp start-nodes) (setf start-nodes (list start-nodes)))
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let ((*graphviz-node-table* (make-hash-table :test #'eq))
          (*print-pretty* nil))
      (format stream "digraph G {~%")
      (format stream "    node [shape = Mrecord, style=filled];~%")
      (dolist (start-node start-nodes)
        (stream-draw-graph start-node stream))
      (format stream "}~%"))))

(defmethod stream-draw-graph :around (node stream)
  (when (null (gethash node *graphviz-node-table*))
    (setf (gethash node *graphviz-node-table*) (gensym "NODE"))
    (call-next-method)
    (loop for predecessor in (predecessors node)
          do (stream-draw-graph predecessor stream)
             (format stream "    ~w -> ~w;~%"
                     (id predecessor) (id node)))))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; methods to draw individual nodes
;;; _________________________________________________________________

(defmethod stream-draw-graph ((node application) stream)
  (format stream "    ~w [fillcolor = tomato, label = \"application ~w\\n~w\"];~%"
          (id node) (name (operator node)) (index-space node)))

(defmethod stream-draw-graph ((node reduction) stream)
  (format stream "    ~w [fillcolor = cornflowerblue, label = \"reduction ~w\\n~w\"];~%"
          (id node) (name (operator node)) (index-space node)))

(defmethod stream-draw-graph ((node fusion) stream)
  (format stream "    ~w [fillcolor = grey, label = \"fusion\\n~w\"];~%"
          (id node) (index-space node)))

(defmethod stream-draw-graph ((node reference) stream)
  (format stream "    ~w [fillcolor = lavender, label = \"reference\\n~w\\n~w\"];~%"
          (id node) (transformation node) (index-space node)))

(defmethod stream-draw-graph ((node repetition) stream)
  (format stream "    ~w [fillcolor = lightseagreen, label = \"repetition\\n~w\"];~%"
          (id node) (index-space node)))

(defmethod stream-draw-graph ((node structured-operand) stream)
  (format stream "    ~w [fillcolor = cyan, label = \"~w\\n~w\"];~%"
          (id node) (element-type node) (index-space node)))

