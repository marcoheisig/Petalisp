;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defvar *graphviz-viewer* "evince")

(defvar *graphviz-format* :pdf)

(defun graphviz (graph-type &rest graph-roots)
  (uiop:with-temporary-file (:pathname image-file)
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots graph-type graph-roots)
     image-file
     :format *graphviz-format*)
    (uiop:run-program
     (list *graphviz-viewer*
           (uiop:native-namestring image-file)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun merge-graphviz-attribute-plists (&rest plists)
    (hash-table-plist
     (plist-hash-table
      (apply #'append plists) :test #'eq)))

  (define-method-combination graphviz-attributes ()
    ((primary ()))
    (print
     `(merge-graphviz-attribute-plists
       ,@(loop for method in primary
               collect `(call-method ,method))))))

(defgeneric graphviz-node-attributes (graph node)
  (:method-combination graphviz-attributes))

(defclass data-flow-graph () ())

(defmethod cl-dot:generate-graph-from-roots
    ((graph (eql 'data-flow-graph))
     objects &optional attributes)
  (cl-dot:generate-graph-from-roots (make-instance graph) objects attributes))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph data-flow-graph)
     (data-structure data-structure))
  (inputs data-structure))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph data-flow-graph)
     (immediate immediate))
  (concatenate 'list
               (call-next-method)
               (kernels immediate)))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph data-flow-graph)
     (kernel kernel))
  (concatenate 'list
               (call-next-method)
               (kernel-references kernel)))

(defmethod cl-dot:graph-object-node
    ((graph data-flow-graph)
     (node t))
  (make-instance 'cl-dot:node
    :attributes (graphviz-node-attributes graph node)))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (node t))
  `(:shape :box :style :filled))

(defmethod graphviz-node-attributes
    ((graph data-flow-graph)
     (data-structure data-structure))
  `(:label ,(format nil "~A~%~A"
                    (class-name (class-of data-structure))
                    (index-space data-structure))))

(defmethod graphviz-graph-plist plist-union
    ((purpose data-flow-graph))
  `(:splines "ortho"))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node t))
  )

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node data-structure))
  `(:label ))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node strided-array-immediate))
  `(:fillcolor "cadetblue1"
               ,@(when-let ((storage (and (= 1 (size node))
                                          (storage node))))
        (let ((*print-right-margin* 60))
          `(:label
            ,(format nil "~A~%~A~%~A"
                     (class-name (class-of node))
                     (index-space node)
                     (let ((*print-length* 8))
                       (format nil "~A" (storage node)))))))))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node application))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (operator node)
                    (index-space node))
    :fillcolor "burlywood1"))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node reduction))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (binary-operator node)
                    (index-space node))
    :fillcolor "beige"))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node fusion))
  `(:fillcolor "cyan3"))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (node reference))
  `(:label ,(format nil "~A~%~A~%~A"
                    (class-name (class-of node))
                    (transformation node)
                    (index-space node))
    :fillcolor "gray"))

(defmethod graphviz-node-plist plist-union
    ((purpose data-flow-graph) (kernel kernel))
  `(:shape "box"
    :fillcolor "skyblue"
    :label
    ,(destructuring-bind (ranges target write sources reads body)
         (ulist-shallow-copy (kernel-blueprint kernel))
       (format nil "
ranges: ~A~%
target: ~A~%
write: ~A~%
sources: ~A~%
reads: ~A~%
body: ~A"
               ranges target write sources reads body))))

(defmethod graphviz-edge-plist plist-union
    ((purpose data-flow-graph) (a kernel) (b immediate))
  `(:style "dashed"))
