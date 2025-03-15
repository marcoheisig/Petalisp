(in-package #:petalisp.core)

(defparameter *valid-inspect-tags*
  '(:graph :ir :partitioning :dependencies :execution))

(defparameter *active-inspect-tags* '())

;; the next four special variables are set by petalisp.graphviz

(defun defined-by-graphviz (&rest args)
  (error "Please load petalisp.graphviz to visualize ~S.~%"
         args))

(defparameter *inspect-graph* 'defined-by-graphviz)

(defparameter *inspect-ir* 'defined-by-graphviz)

(defparameter *inspect-partitioning* 'defined-by-graphviz)

(defparameter *inspect-dependencies* 'defined-by-graphviz)

(defmacro with-inspection ((&rest tags) &body body)
  "Visualize intermediate steps of the evaluation.  Valid TAGS are:

:GRAPH - Show the data flow graph.

:IR - Show the intermediate representation.

:PARTITIONING - Show the partitioned intermediate representation.

:DEPENDENCIES - Show the dependency graph."
  (dolist (tag tags)
    (unless (member tag *valid-inspect-tags*)
      (error "Invalid inspect tag: ~S" tag)))
  `(let ((*active-inspect-tags* ',tags))
     ,@body))
