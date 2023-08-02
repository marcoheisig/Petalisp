;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass partitioning-graph (petalisp-graph) ())

(defclass partitioning-edge (petalisp-edge) ())

(defclass reader-edge (partitioning-edge) ())

(defclass writer-edge (partitioning-edge) ())

(defmethod graphviz-default-graph ((node petalisp.ir:kernel-shard))
  'partitioning-graph)

(defmethod graphviz-default-graph ((node petalisp.ir:buffer-shard))
  'partitioning-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connectivity

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph partitioning-graph)
     (buffer-shard petalisp.ir:buffer-shard))
  (let ((edge (make-instance 'writer-edge)))
    (labels ((gather-writers (buffer-shard)
               (let ((split (petalisp.ir:buffer-shard-split buffer-shard)))
                 (append
                  (loop for kernel-shard in (petalisp.ir:buffer-shard-writers buffer-shard)
                        for edge-number from 0
                        collect
                        (make-instance 'cl-dot:attributed
                          :attributes
                          (graphviz-edge-attributes graph edge kernel-shard buffer-shard edge-number)
                          :object kernel-shard
                          :target-port (buffer-shard-port buffer-shard)))
                  (when split (gather-writers (petalisp.ir:split-left-child split)))
                  (when split (gather-writers (petalisp.ir:split-right-child split)))))))
      (gather-writers buffer-shard))))

(defmethod cl-dot:graph-object-points-to
    ((graph partitioning-graph)
     (buffer-shard petalisp.ir:buffer-shard))
  (let ((edge (make-instance 'reader-edge)))
    (labels ((gather-readers (buffer-shard)
               (let ((split (petalisp.ir:buffer-shard-split buffer-shard)))
                 (append
                  (loop for kernel-shard in (petalisp.ir:buffer-shard-readers buffer-shard)
                        for edge-number from 0
                        collect
                        (make-instance 'cl-dot:attributed
                          :attributes
                          (graphviz-edge-attributes graph edge buffer-shard kernel-shard edge-number)
                          :object kernel-shard
                          :source-port (buffer-shard-port buffer-shard)))
                  (when split (gather-readers (petalisp.ir:split-left-child split)))
                  (when split (gather-readers (petalisp.ir:split-right-child split)))))))
      (append
       (gather-readers buffer-shard)))))

(defmethod cl-dot:graph-object-knows-of
    ((graph partitioning-graph)
     (node petalisp.ir:kernel-shard))
  (mapcar #'petalisp.ir:buffer-shard-primogenitor
          (append (petalisp.ir:kernel-shard-targets node)
                  (petalisp.ir:kernel-shard-sources node))))

(defmethod graphviz-edge-attributes
    ((graph partitioning-graph)
     (edge reader-edge)
     (to petalisp.ir:buffer-shard)
     (from petalisp.ir:kernel-shard)
     (n t))
  `(:color "darkblue"))

(defmethod graphviz-edge-attributes
    ((graph partitioning-graph)
     (edge writer-edge)
     (from petalisp.ir:kernel-shard)
     (to petalisp.ir:buffer-shard)
     (n t))
  `(:color "darkred"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clusters

(defvar *kernel-cluster-table*)

(defmethod cl-dot:generate-graph-from-roots :around
    ((graph partitioning-graph) roots &optional attributes)
  (declare (ignore graph roots attributes))
  (let ((*kernel-cluster-table* (make-hash-table)))
    (call-next-method)))

(defun kernel-cluster (kernel)
  (alexandria:ensure-gethash
   kernel
   *kernel-cluster-table*
   (make-instance 'cl-dot:cluster
     :attributes
     (list :label (format nil "~A" kernel)
           :penwidth 2.0
           :color "gray"))))

(defmethod cl-dot:graph-object-cluster
    ((graph partitioning-graph)
     (kernel-shard petalisp.ir:kernel-shard))
  (kernel-cluster
   (petalisp.ir:kernel-shard-kernel kernel-shard)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod cl-dot:graph-object-node
    ((graph partitioning-graph)
     (node petalisp.ir:kernel-shard))
  (make-instance 'cl-dot:node
    :attributes
    `(:label
      (:html () ,(kernel-shard-label node))
      ,@(graphviz-node-attributes graph node))))

(defun kernel-shard-label (kernel-shard)
  (let* ((shape (petalisp.ir:kernel-shard-iteration-space kernel-shard)))
    `(:table
      ((:border "0") (:cellborder "0") (:cellspacing "0") (:cellpadding "0"))
      (:tr () (:td () ,(format nil "~A" shape))))))

(defmethod cl-dot:graph-object-node
    ((graph partitioning-graph)
     (node petalisp.ir:buffer-shard))
  (make-instance 'cl-dot:node
    :attributes
    `(:label
      (:html () ,(buffer-shard-label node))
      ,@(graphviz-node-attributes graph node))))

(defun buffer-shard-label (buffer-shard)
  (let* ((split (petalisp.ir:buffer-shard-split buffer-shard))
         (shape (petalisp.ir:buffer-shard-shape buffer-shard))
         (domain (petalisp.ir:buffer-shard-domain buffer-shard))
         (storage (petalisp.ir:buffer-shard-storage buffer-shard))
         (parent (petalisp.ir:buffer-shard-parent buffer-shard)))
    `(:table
      ((:port ,(buffer-shard-port buffer-shard))
       (:cellborder "0"))
      (:tr () (:td () (:b () "domain:") ,(format nil "~A" domain)))
      (:tr () (:td () (:b () "shape:") ,(format nil "~A" shape)))
      ,@(when (and storage
                   (or (not parent)
                       (not (petalisp.ir:buffer-shard-storage parent))))
          `((:tr () (:td () (:b () "strides:") ,(format nil "~A" (petalisp.ir:storage-strides storage))))
            (:tr () (:td () (:b () "offset:") ,(format nil "~A" (petalisp.ir:storage-offset storage))))
            (:tr () (:td () (:b () "size:") ,(format nil "~A" (petalisp.ir:storage-size storage))))
            (:tr () (:td () (:b () "type:") ,(format nil "~A"
                                                     (typo:ntype-type-specifier
                                                      (petalisp.ir:storage-ntype storage)))))
            (:tr
             ()
             (:td
              ()
              (:table
               ((:border "1")
                (:cellspacing "0")
                (:cellpadding "0")
                (:cellborder "0")
                (:fixedsize "TRUE")
                (:bgcolor "#ABCEF3"))
               ,@(multiple-value-bind (nrows ncolumns)
                     (case (petalisp:shape-rank shape)
                       (0 (values 1 1))
                       (1 (values (petalisp:range-size (petalisp:shape-range shape 0)) 1))
                       (otherwise
                        (values (petalisp:range-size (petalisp:shape-range shape 0))
                                (petalisp:range-size (petalisp:shape-range shape 1)))))
                   (loop for row below nrows
                         collect
                         `(:tr ()
                               ,@(loop for column below ncolumns
                                       collect `(:td ((:width "5") (:height "5"))))))))))))
      ,@(when split
          `((:tr
             ()
             (:td
              ()
              ,(let* ((axis (petalisp.ir:split-axis split))
                      (left-child (petalisp.ir:split-left-child split))
                      (right-child (petalisp.ir:split-right-child split)))
                 `(:table
                   ((:border "0"))
                   ,@(case axis
                       (0
                        `((:tr () (:td () ,(buffer-shard-label left-child)))
                          (:tr () (:td () ,(buffer-shard-label right-child)))))
                       (1
                        `((:tr ()
                               (:td () ,(buffer-shard-label left-child))
                               (:td () ,(buffer-shard-label right-child)))))
                       (otherwise
                        `((:tr ()
                               (:td () ,(buffer-shard-label left-child))
                               (:td () ,(format nil "axis ~D split" axis))
                               (:td () ,(buffer-shard-label right-child)))))))))))))))

(defun buffer-shard-port (buffer-shard)
  (format nil "buffer~Dshard~{~:[R~;L~]~}"
          (petalisp.ir:buffer-number (petalisp.ir:buffer-shard-buffer buffer-shard))
          (petalisp.ir:buffer-shard-path buffer-shard)))
