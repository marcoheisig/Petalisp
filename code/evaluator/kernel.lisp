;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel ()
  ((recipe :type data-structure :accessor recipe)
   (cost :type non-negative-integer :accessor cost)
   (users :type list :initform nil :accessor users)
   (dependencies :type list)
   (target :type immediate :accessor target))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))

(defun make-kernel (recipe dependencies)
  (make-instance 'kernel
    :recipe recipe
    :cost (size recipe)
    :dependencies dependencies
    :target (make-instance 'strided-array-immediate
              :index-space (index-space recipe)
              :element-type (element-type recipe)
              :transformation (make-identity-transformation (dimension recipe)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphviz visualization of kernel graphs

(defclass <kernel-graph> (<data-flow-graph>) ())

(defmethod graphviz-successors ((purpose <kernel-graph>) (kernel kernel))
  (cons (recipe kernel) (users kernel)))

(defmethod graphviz-successors ((purpose <kernel-graph>) (data-structure data-structure))
  (inputs data-structure))

(defmethod graphviz-node-plist append-plist ((purpose <kernel-graph>) (kernel kernel))
  `(:label ,(format nil "kernel ~A" (index-space (target kernel)))
    :shape "octagon"
    :fontsize 20))

(defmethod graphviz-edge-plist append-plist ((purpose <kernel-graph>) (k1 kernel) (k2 kernel))
  `(:weight 0 :dir "forward" :style "dashed" :penwidth 2.0))

