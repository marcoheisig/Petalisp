;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; methods to draw individual nodes

(defgeneric printworthy? (object)
  (:method ((object t)) t)
  (:method ((slot standard-effective-slot-definition))
    (not (member (slot-definition-name slot) '(ranges predecessors)))))

(defmethod stream-draw-graph ((node standard-object) stream)
  (let ((class (class-of node)))
    (format stream "    ~a [label=\"" (id node))
    (format stream "~a\\n" (string-downcase (class-name class)))
    (loop for slot in (class-slots class)
          when (printworthy? slot) do
            (format
             stream "~a: ~a\\n"
             (string-downcase (slot-definition-name slot))
             (remove-if
              (lambda (x) (member x '(#\# #\< #\>)))
              (format nil "~a" (slot-value-using-class class node slot)))))
    (format stream "\"]~%")))
