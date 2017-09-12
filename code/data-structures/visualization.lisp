;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass <data-flow-graph> (<graph>) ())

(defmethod graphviz-successors ((purpose <data-flow-graph>) (node data-structure))
  (inputs node))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (node elaboration))
  (ensure-list (recipe node)))

(defmethod graphviz-node-plist plist-append ((purpose <data-flow-graph>) (node data-structure))
  (list :label (format nil "~A~%~A"
                       (class-name (class-of node))
                       (index-space node))
        :shape "box"
        :style "filled"))

(defmethod graphviz-node-plist plist-append ((purpose <data-flow-graph>) (node application))
  (list :label (format nil "~A~%~A~%~A"
                       (class-name (class-of node))
                       (operator node)
                       (index-space node))
        :fillcolor "indianred1"))

(defmethod graphviz-node-plist plist-append ((purpose <data-flow-graph>) (node reduction))
  (list :label (format nil "~A~%~A~%~A"
                      (class-name (class-of node))
                      (operator node)
                      (index-space node))
        :fillcolor "indianred3"))

(defmethod graphviz-node-plist plist-append ((purpose <data-flow-graph>) (node reference))
  (list :label (format nil "~A~%~A~%~A"
                       (class-name (class-of node))
                       (transformation node)
                       (index-space node))
        :fillcolor "gray"))

(defmethod graphviz-node-plist plist-append ((purpose <data-flow-graph>) (node elaboration))
  (list :label (format nil "~A~%~A~%~A"
                       (class-name (class-of node))
                       (data node)
                       (index-space node))
        :fillcolor (if (dependencies node) "green" "white")))

(defmethod view ((object data-structure))
  (with-temporary-file (:stream stream :pathname dotfile :direction :output)
    (graphviz-draw-graph '<data-flow-graph> (list object) stream)
    :close-stream
    (with-temporary-file (:pathname imagefile)
      (run-program (list "dot" "-Tpdf" "-o"
                         (native-namestring imagefile)
                         (native-namestring dotfile)))
      (run-program (list "evince" (native-namestring imagefile))))))

(defmethod graphviz-draw-graph
    :around ((purpose <data-flow-graph>) graph-roots &optional stream)
  (let ((*print-case* :downcase))
    (call-next-method purpose graph-roots stream)))
